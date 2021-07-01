package cps.macros.forest.application

import scala.annotation.tailrec
import scala.quoted._


import cps._
import cps.macros._
import cps.macros.forest._
import cps.macros.misc._


trait ApplyArgRecordScope[F[_], CT]:

  thisTreeTransform: TreeTransformScope[F,CT] =>

  import qctx.reflect._


  sealed trait ApplyArgRecord:
    def term: Term
    def index: Int
    def identArg(existAsync:Boolean): Term
    def isAsync: Boolean
    def hasShiftedLambda: Boolean

    // means, that the value of argument is independed from the order of evaluation of aguments in a function.
    def noOrderDepended: Boolean

    // usePrepend: means that args is collected as 'append(...append(...  funcall(..ident...)))`
    //  when some of arguments is async, we should in 'first part'(i.e. in append) - calculate
    //  argument itself, to preserve order of evaluation.  When we have no async arguments, than
    //  we can use unchanged term to be an argument
    def usePrepend(existsAsync:Boolean): Boolean = isAsync || (existsAsync && !noOrderDepended)

    // means, that the value of argument can depend from the order of evaluation of aguments in a function.
    def isOrderDepended = !noOrderDepended

    def shift(): ApplyArgRecord

    def append(tree: CpsTree): CpsTree

  case class ApplyArgRepeatRecord(
       term: Repeated,
       index: Int,
       elements: List[ApplyArgRecord],
       seqTypeTree: TypeTree
  ) extends ApplyArgRecord {
    override def usePrepend(existsAsync:Boolean): Boolean = elements.exists(_.usePrepend(existsAsync))
    override def identArg(existsAsync:Boolean): Term =
      if (usePrepend(existsAsync))
          Typed(Repeated(elements.map(_.identArg(existsAsync)),term.elemtpt), seqTypeTree)
      else if (hasShiftedLambda)
          Typed(Repeated(elements.map(_.identArg(existsAsync)),shiftedElemTpt), shiftSeqTypeTree)
      else
          term
    override def isAsync = elements.exists(_.isAsync)
    override def hasShiftedLambda = elements.exists(_.hasShiftedLambda)
    override def noOrderDepended = elements.forall(_.noOrderDepended)
    override def shift() = copy(elements = elements.map(_.shift()))

    override def append(tree: CpsTree): CpsTree =
       if (elements.isEmpty)
         tree
       else
         elements.foldRight(tree){(e,s) =>
           e match
            case _: ApplyArgNoPrecalcTermRecord =>
                  s
            case e1: ApplyArgPrecalcTermRecord =>
                  e.append(s)
            case _: ApplyArgLambdaRecord => s
            case _ => // incorrect warning
               throw MacroError("Impossible: repeated inside repeated",term.asExpr)
         }

    private def shiftedElemTpt: TypeTree = 
         if (term.elemtpt.tpe =:= TypeRepr.of[Any]) then
           // this can be dynamic, leave as is
           term.elemtpt
         else
           shiftedLambdaTypeTree(term.elemtpt)

    private def shiftSeqTypeTree: TypeTree = 

         def notFoundBackup: TypeTree =
              report.warning(s"strange type repeated: $seqTypeTree", term.asExpr)
              seqTypeTree

         seqTypeTree match
           case Applied(rep,args) =>
                 if (rep.tpe.typeSymbol == defn.RepeatedParamClass) then
                    Inferred(rep.tpe.appliedTo(shiftedElemTpt.tpe))
                 else
                    notFoundBackup
           case Inferred() =>
                 seqTypeTree.tpe match
                    case AppliedType(rep,List(v)) =>
                          if (rep.typeSymbol == defn.RepeatedParamClass) then
                              Inferred(rep.appliedTo(shiftedElemTpt.tpe))
                          else
                              notFoundBackup
           case _ =>
                 notFoundBackup

  }

  case class ApplyArgNoPrecalcTermRecord(
       term: Term,
       index: Int
  ) extends ApplyArgRecord
  {
     def isAsync = false
     def hasShiftedLambda: Boolean = false
     def noOrderDepended = termIsNoOrderDepended(term)
     def identArg(existsAsync: Boolean): Term = term
     def shift(): ApplyArgRecord = this
     override def append(tree: CpsTree): CpsTree = tree
  }

  case class ApplyArgPrecalcTermRecord(
       term: Term,
       index: Int,
       termCpsTree: CpsTree,
       valDef: ValDef,
       ident: Term
  ) extends ApplyArgRecord
  {
     def isAsync = termCpsTree.isAsync
     def hasShiftedLambda: Boolean = false
     def noOrderDepended = termIsNoOrderDepended(term)
     def identArg(existsAsync: Boolean): Term =
            if (existsAsync) ident else term
     def shift(): ApplyArgRecord = this
     override def append(tree: CpsTree): CpsTree =
        ValCpsTree(valDef, termCpsTree, tree)
  }


  case class ApplyArgLambdaRecord(
       term: Term,   // Lambda,  see coding of Lambda in Tasty Reflect.
       index: Int,
       cpsBody: CpsTree,
       shifted: Boolean
  ) extends ApplyArgRecord {

       def hasShiftedLambda: Boolean = cpsBody.isAsync

       def isAsync: Boolean = false

       def noOrderDepended: Boolean = true

       def identArg(existsAsync:Boolean): Term =
         if (cpsCtx.flags.debugLevel >= 15) then
            cpsCtx.log(s"ApplyArgLambdaRecord::identArg, cpsBody=${cpsBody}")
            cpsCtx.log(s"ApplyArgLambdaRecord::identArg, hasShiftedLambda=${hasShiftedLambda}, shifted=${shifted}")
         if (hasShiftedLambda || shifted)
            val (params, body) = term match
              case Lambda(params, body) => (params, body)
              case _ =>
                 throw MacroError(s"Lambda expexted, we have ${term.asExpr.show}",term.asExpr)
            shiftedArgExpr(existsAsync, term.tpe, params, body)
         else
            term


       def shiftedArgExpr(existsAsync:Boolean, identType: TypeRepr, params: List[ValDef], body:Term): Term =
         identType match
            case MethodType(paramNames, paramTypes, resType) =>
                val mt = shiftedMethodType(paramNames, paramTypes, resType)
                createAsyncLambda(mt, params, Symbol.spliceOwner)
            case ft@AppliedType(tp,tparams) =>
                  if (ft.isFunctionType) {
                      //val paramTypes = tparams.dropRight(1).map(typeOrBoundsToType(_,false))
                      val paramTypes = tparams.dropRight(1)
                      val resType = tparams.last
                      val paramNames = params.map(_.name)
                      val mt = shiftedMethodType(paramNames, paramTypes, resType)
                      createAsyncLambda(mt, params, Symbol.spliceOwner)
                  } else if (tp <:< partialFunctionType ) {
                      val (tIn, tOut) = tparams match
                         case tIn::tOut::Nil => (tIn, tOut)
                         case _ =>
                           throw MacroError(s"PartialFunction should have 2 type parameters, we have $tparams", term.asExpr)
                      val matchTerm = body match
                         case m@Match(_,_) => m
                         case _ =>
                           throw MacroError(s"PartialFunction should be represented as Match term, we have $body", posExprs(body,term))
                      createAsyncPartialFunction(tIn, tOut, matchTerm, params)
                  } else {
                      throw MacroError(s"FunctionType expected, we have ${tp}", term.asExpr)
                  }
            case at@AnnotatedType(underlying,anon) =>
                  shiftedArgExpr(existsAsync, underlying, params, body)
            case other =>
                  // TODO: logging compiler interface instead println
                  println(s"MethodType expected, we have ${term.tpe}")
                  println(s"term.show = ${term.show}")
                  println(s"term.body = ${term}")
                  println(s"mt = ${other}")
                  throw MacroError(s"methodType expected for ${term.asExpr.show}, we have $other",term.asExpr)

       def shift(): ApplyArgRecord = copy(shifted=true)

       def append(a: CpsTree): CpsTree = a

       private def createAsyncPartialFunction(from: TypeRepr, to: TypeRepr, body: Match, params: List[ValDef]): Term =
         val toInF = TypeRepr.of[F].appliedTo(List(to))
         val fromType = from
         val matchVar = body.scrutinee
         val paramNames = params.map(_.name)

         if (cpsCtx.flags.debugLevel >= 15)
             println(s"createAsyncPartialFunction: from = $from, to=$to")
             println(s"toInF=$toInF")

         def newCheckBody(inputVal:Term):Term =

            val casePattern = '{
                 ${inputVal.asExpr} match
                    case _ => false
            }.asTerm

            @tailrec
            def transformCases(rest:List[CaseDef],
                               acc:List[CaseDef],
                               wasDefault: Boolean):List[CaseDef]=
              rest match
                case h::t =>
                     val nh = rebindCaseDef(h, Literal(BooleanConstant(true)), Map.empty, false, Symbol.spliceOwner)
                     transformCases(t, nh::acc, wasDefault)
                case Nil =>
                      val lastCase = casePattern match
                        case Inlined(_,List(),Match(x,cases)) => cases.head
                        case Match(x,cases) => cases.head
                        case _ =>
                            throw MacroError("Internal error: case pattern should be Inlined(_,_,Match) pr Match, we have $casePattern",posExpr(term))
                      ;
                      (lastCase::acc).reverse

            Match.copy(body)(inputVal, transformCases(body.cases,Nil,false))
            //Match(inputVal, transformCases(body.cases,Nil,false))


         def termCast[E:Type](term: Term): Expr[E] =
            term.asExprOf[E]


         val r = fromType.asType match
           case '[ftt] =>
             toInF.asType match
               case '[ttt] =>
                  ('{ new PartialFunction[ftt,ttt] {
                       override def isDefinedAt(x1:ftt):Boolean =
                          ${ newCheckBody('x1.asTerm).asExprOf[Boolean] }
                       override def apply(x2:ftt): ttt =
                          ${ val nBody = cpsBody.transformed
                             nBody match
                               case m@Match(scr,caseDefs) =>
                                 val b0 = Map(matchVar.symbol -> 'x2.asTerm)
                                 val nCaseDefs = caseDefs.map( cd =>
                                                    rebindCaseDef(cd, cd.rhs, b0, true, Symbol.spliceOwner))
                                 val nTerm = Match('x2.asTerm, nCaseDefs)
                                 termCast(nTerm)
                               case _ =>
                                 throw MacroError(
                                   s"assumed that transformed match is Match, we have $nBody",
                                   posExprs(term)
                                 )
                           }
                     }
                   }).asTerm
               case _ =>
                  throw MacroError("Can't skolemize $toInF", posExprs(term) )
           case _ =>
             throw MacroError("Can't skolemize $fromType", posExprs(term) )

         r

       private def createAsyncLambda(mt: MethodType, params: List[ValDef], owner: Symbol): Term =
         val transformedBody = cpsBody.transformed
         Lambda(owner, mt, (owner,args) => changeArgs(params,args,transformedBody,owner).changeOwner(owner))

       private def rebindCaseDef(caseDef:CaseDef,
                                 body: Term,
                                 assoc: Map[Symbol, Term],
                                 processBody: Boolean,
                                 owner: Symbol): CaseDef = {


         def rebindPatterns(pattern: Tree, map:Map[Symbol,Term]): (Tree, Map[Symbol, Term]) = {
           pattern match
             case bd@Bind(name,pat1) =>
                val bSym = bd.symbol
                val nBSym = Symbol.newBind(Symbol.spliceOwner, name,bSym.flags, Ref(bSym).tpe.widen)
                val nMap = map.updated(bSym, Ref(nBSym))
                val (nPat1, nMap1) = rebindPatterns(pat1, nMap)
                (Bind(nBSym,nPat1), nMap1)
             case u@Unapply(fun, implicits, patterns) =>
                val s0: (List[Tree], Map[Symbol,Term]) = (List.empty, Map.empty)
                val sR = patterns.foldLeft(s0){ (s,e) =>
                   val (ep, em) = rebindPatterns(e,s._2)
                   (ep::s._1, em)
                }
                val nPatterns = sR._1.reverse
                (Unapply.copy(u)(fun, implicits, nPatterns), sR._2)
             case other =>
                (other, map)
         }

         val (nPattern, newBindings) = rebindPatterns(caseDef.pattern, assoc)
         val nGuard = caseDef.guard.map( TransformUtil.changeSyms(newBindings, _, owner ) )
         val nBody = if (processBody) TransformUtil.changeSyms(newBindings, body, owner) else body
         CaseDef(nPattern, nGuard, nBody)
       }

       private def changeArgs(params:List[ValDef], nParams:List[Tree], body: Term, owner: Symbol): Term =
         TransformUtil.substituteLambdaParams(params, nParams, body, owner)


       private def changeIdent(body:Term, oldSym: Symbol, newSym: Symbol, owner: Symbol): Term =
         TransformUtil.changeSyms(Map(oldSym->Ref(newSym)), body, owner)

  }

  case class ApplyArgNamedRecord(term: NamedArg, name: String, nested: ApplyArgRecord )
     extends ApplyArgRecord {

       def index: Int = nested.index

       def hasShiftedLambda: Boolean = nested.hasShiftedLambda
       def isAsync: Boolean = nested.isAsync
       def noOrderDepended = nested.noOrderDepended
       def identArg(existsAsync: Boolean): Term = NamedArg(name, nested.identArg(existsAsync))
       def shift(): ApplyArgRecord = copy(nested=nested.shift())
       def append(a: CpsTree): CpsTree = nested.append(a)

  }



  case class ApplyArgByNameRecord(term: Term,
                                  index: Int,
                                  cpsTree: CpsTree,
                                  shifted: Boolean) extends ApplyArgRecord {
    def identArg(existsAsync: Boolean): Term =
      if !shifted then
         term
      else
         val mt = MethodType(List())(_ => List(), _ => TypeRepr.of[F].appliedTo(List(term.tpe.widen)))
         Lambda(Symbol.spliceOwner,mt, (owner,args) => cpsTree.transformed.changeOwner(owner))

    def isAsync: Boolean = cpsTree.isAsync
    def hasShiftedLambda: Boolean = shifted
    def noOrderDepended: Boolean = true
    def shift() = copy(shifted = true)
    def append(tree: CpsTree): CpsTree = tree

  }


  case class ApplyArgInlinedRecord(tree: InlinedCpsTree, nested: ApplyArgRecord )
     extends ApplyArgRecord {
       def index: Int = nested.index
       def term: Term =
              Inlined(tree.origin.call, tree.bindings, nested.term)
       def hasShiftedLambda: Boolean = nested.hasShiftedLambda
       def isAsync: Boolean = nested.isAsync
       def noOrderDepended = nested.noOrderDepended
       def identArg(existsAsync:Boolean): Term = nested.identArg(existsAsync)
       def shift(): ApplyArgRecord = copy(nested=nested.shift())
       def append(a: CpsTree): CpsTree =
             val na = nested.append(a)
             if (na eq a)
                a
             else
                InlinedCpsTree(tree.origin, tree.bindings, na)
  }


  def termIsNoOrderDepended(x:Term): Boolean =
    x match {
      case Literal(_) => true
      case Ident(_) => if (x.symbol.isValDef) then
                         x.symbol.flags.is(Flags.Mutable)
                       else if x.symbol.isDefDef then
                         true
                       else if x.symbol.isBind then
                         true
                       else
                         false
      case _ => false
    }


