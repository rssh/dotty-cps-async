package cps.macros.forest.application

import scala.annotation.tailrec
import scala.quoted.*
import scala.collection.immutable.Queue
import scala.util.control.NonFatal
import cps.*
import cps.macros.*
import cps.macros.common.*
import cps.macros.forest.*
import cps.macros.forest.application.ApplicationShiftType.{CPS_AWAIT, CPS_DEFERR_TO_PLUGIN}
import cps.macros.misc.*



trait ApplyArgRecordScope[F[_], CT, CC<:CpsMonadContext[F]]:

  thisTreeTransform: TreeTransformScope[F,CT, CC] =>

  import qctx.reflect._

  case class ApplyArgsSummaryPropertiesStep1(
    size: Int,
    hasAsync: Boolean,
    hasShiftedLambda: Boolean,
    shouldBeChangedSync: Boolean,
    cpsDirectArg: Option[Term],
  ):

    def merge(other: ApplyArgRecord): ApplyArgsSummaryPropertiesStep1 =
      ApplyArgsSummaryPropertiesStep1(
        size + 1,
        hasAsync || other.isAsync,
        hasShiftedLambda || other.hasShiftedLambda,
        shouldBeChangedSync || other.shouldBeChangedSync,
        if (cpsDirectArg.isEmpty) then
          if (other.isCpsDirect) then Some(other.term)  else None
        else
          cpsDirectArg
      )

    def mergeSeq(seq: Seq[ApplyArgRecord]): ApplyArgsSummaryPropertiesStep1 =
      seq.foldLeft(this)((s,e) => s.merge(e))

    def mergeSeqSeq(seqSeq: Seq[Seq[ApplyArgRecord]]): ApplyArgsSummaryPropertiesStep1 =
      seqSeq.foldLeft(this)((s,e) => s.mergeSeq(e))


  object ApplyArgsSummaryPropertiesStep1:

     def mergeSeqSeq(args: Seq[Seq[ApplyArgRecord]]): ApplyArgsSummaryPropertiesStep1 =
        val zero = ApplyArgsSummaryPropertiesStep1(0,false,false,false,None)
        zero.mergeSeqSeq(args)

  case class ApplyArgsSummaryProperties(
        step1: ApplyArgsSummaryPropertiesStep1,
        usePrepend: Boolean
  ):

     def hasAsync: Boolean = step1.hasAsync
     def hasShiftedLambda: Boolean = step1.hasShiftedLambda
     def shouldBeChangedSync: Boolean = step1.shouldBeChangedSync
     def cpsDirectArg: Option[Term] = step1.cpsDirectArg

     def mergeStep2SeqSeq(seqSeq: Seq[Seq[ApplyArgRecord]]): ApplyArgsSummaryProperties =
       seqSeq.foldLeft(this)((s,e)=> s.mergeStep2Seq(e))

     def mergeStep2Seq(seq: Seq[ApplyArgRecord]): ApplyArgsSummaryProperties =
       seq.foldLeft(this)((s,e)=> s.mergeStep2(e))

     def mergeStep2(r: ApplyArgRecord): ApplyArgsSummaryProperties =
       ApplyArgsSummaryProperties(step1=step1, usePrepend = usePrepend || r.usePrepend(step1.hasAsync))

  object ApplyArgsSummaryProperties:

     def mergeSeqSeq(seqSeq: Seq[Seq[ApplyArgRecord]]): ApplyArgsSummaryProperties =
        val step1 = ApplyArgsSummaryPropertiesStep1.mergeSeqSeq(seqSeq)
        ApplyArgsSummaryProperties(step1,false).mergeStep2SeqSeq(seqSeq)


  sealed trait ApplyArgRecord:
    def term: Term
    def index: Int
    def identArg(existAsync:Boolean): Term
    def isAsync: Boolean
    def hasShiftedLambda: Boolean

     // means, that the value should be changed during regeneration, event it is sync.
    def shouldBeChangedSync: Boolean

    // means, that the value of argument is independed from the order of evaluation of aguments in a function.
    def noOrderDepended: Boolean

    // means, that this is a CpsDirect context parameter
    def isCpsDirect: Boolean

    // usePrepend: means that args is collected as 'append(...append(...  funcall(..ident...)))`
    //  when some of arguments is async, we should in 'first part'(i.e. in append) - calculate
    //  argument itself, to preserve order of evaluation.  When we have no async arguments, than
    //  we can use unchanged term to be an argument
    def usePrepend(existsAsync:Boolean): Boolean = isAsync || (existsAsync && !noOrderDepended)

    // means, that the value of argument can depend from the order of evaluation of aguments in a function.
    def isOrderDepended = !noOrderDepended

    def shift(): ApplyArgRecord

    def withRuntimeAwait(runtimeAwait: Term): ApplyArgRecord

    def append(tree: CpsTree): CpsTree


  def  applyRuntimeAwait(runtimeAwait: Term, arg:Term, resultType: TypeRepr): Term =
      val m = cpsCtx.monad.asTerm
      val mc = cpsCtx.monadContext.asTerm
      //TODO:  think how to setup pos.
      Apply(
       Apply(
         TypeApply(Select.unique(runtimeAwait, "await"),List(Inferred(resultType))),
         List(arg)
       ),
       List(mc)
      )



  case class ApplyArgRepeatRecord(
       term: Term, // Typed(Repeated(...), TypeTree) or Repeated(...)
       repeatedTerm: Repeated,
       index: Int,
       elements: List[ApplyArgRecord],
       seqTypeTree: TypeTree
  ) extends ApplyArgRecord {
    override def usePrepend(existsAsync:Boolean): Boolean = elements.exists(_.usePrepend(existsAsync))

    override def identArg(existsAsync:Boolean): Term = {
      val retval = if (usePrepend(existsAsync))
          Typed(Repeated(elements.map(_.identArg(existsAsync)),repeatedTerm.elemtpt), seqTypeTree)
      else if (hasShiftedLambda)
          Typed(Repeated(elements.map(_.identArg(existsAsync)),shiftedElemTpt), shiftSeqTypeTree)
      else
          term
      retval
    }

    override def isAsync = elements.exists(_.isAsync)
    override def hasShiftedLambda = elements.exists(_.hasShiftedLambda)
    override def isCpsDirect: Boolean = false
    override def noOrderDepended = elements.forall(_.noOrderDepended)
    override def shouldBeChangedSync:Boolean = elements.exists(_.shouldBeChangedSync)
    override def shift() = copy(elements = elements.map(_.shift()))

    override def withRuntimeAwait(runtimeAwait: Term): ApplyArgRecord =
       copy(elements = elements.map(_.withRuntimeAwait(runtimeAwait)))

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
         if (repeatedTerm.elemtpt.tpe =:= TypeRepr.of[Any]) then
           // this can be dynamic, leave as is
           repeatedTerm.elemtpt
         else
           shiftedLambdaTypeTree(repeatedTerm.elemtpt)

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
       index: Int,
       isChanged: Boolean,
       override val isCpsDirect: Boolean
  ) extends ApplyArgRecord
  {
     def isAsync = false
     def hasShiftedLambda: Boolean = false
     def noOrderDepended = termIsNoOrderDepended(term)
     override def shouldBeChangedSync:Boolean = isChanged
     override def identArg(existsAsync: Boolean): Term = term
     override def shift(): ApplyArgRecord = this
     override def withRuntimeAwait(runtimeAwait: qctx.reflect.Term): ApplyArgRecord = this
     override def append(tree: CpsTree): CpsTree = tree
  }

  case class ApplyArgPrecalcTermRecord(
       term: Term,
       index: Int,
       termCpsTree: CpsTree,
       valDef: ValDef,
       ident: Term,
        override val isCpsDirect: Boolean
  ) extends ApplyArgRecord
  {
     def isAsync = termCpsTree.isAsync
     def hasShiftedLambda: Boolean = false
     def noOrderDepended = termIsNoOrderDepended(term)
     def shouldBeChangedSync:Boolean = termCpsTree.isChanged
     
     def identArg(existsAsync: Boolean): Term =
            if (existsAsync) then
              ident 
            else if termCpsTree.isChanged then
              termCpsTree.syncOrigin.get
            else 
              term

     override def shift(): ApplyArgRecord = this
     override def withRuntimeAwait(runtimeAwait: qctx.reflect.Term): ApplyArgRecord = this
     override def append(tree: CpsTree): CpsTree =
        ValCpsTree(termCpsTree.owner, valDef, termCpsTree, tree)
  }


  case class ApplyArgLambdaRecord(
       term: Term,   // Lambda,  see coding of Lambda in Tasty Reflect.
       index: Int,
       cpsBody: CpsTree,
       optShiftType: Option[ApplicationShiftType],
       optRuntimeAwait: Option[Term],
       existsLambdaUnshift: Boolean,
       owner: Symbol
  ) extends ApplyArgRecord {

       def hasShiftedLambda: Boolean = cpsBody.isAsync && !existsLambdaUnshift

       def isAsync: Boolean = false

       def isCpsDirect: Boolean = false

       def noOrderDepended: Boolean = true

       def shouldBeChangedSync:Boolean = cpsBody.isChanged || (cpsBody.isAsync && existsLambdaUnshift )

       def identArg(existsAsync:Boolean): Term =
         generateLambda(existsAsync, false)

       def generateLambda(existsAsync: Boolean, allowUncontext: Boolean): Term =   
         if (cpsCtx.flags.debugLevel >= 15) then
            cpsCtx.log(s"ApplyArgLambdaRecord::identArg, cpsBody=${cpsBody}")
            cpsCtx.log(s"ApplyArgLambdaRecord::identArg, hasShiftedLambda=${hasShiftedLambda}")
         if (hasShiftedLambda || optShiftType.isDefined) then
            val shiftType = optShiftType.getOrElse(ApplicationShiftType.CPS_ONLY)
            val (params, body) = extractParamsAndBody()
            shiftedArgExpr(existsAsync, shiftType, term.tpe, params, body)
         else if (shouldBeChangedSync) then
            val (params, body) = extractParamsAndBody()
            val paramNames = params.map(_.name)
            val paramTypes = params.map(_.tpt.tpe)
            cpsBody.syncOrigin match
              case Some(syncBody) =>
                 if (cpsBody.isChanged) then
                    val methodKind = if (term.tpe.isContextFunctionType && !allowUncontext) MethodTypeKind.Contextual else MethodTypeKind.Plain
                    //if (term.tpe.isContextFunctionType && !allowUncontext) then
                    //  val mt = MethodType(MethodTypeKind.Contextual)(paramNames)(_ => paramTypes, _ => syncBody.tpe.widen)
                    //  println(s"context cpsBody = ${cpsBody}, ")
                    //  throw MacroError("Can't transform context function: TastyAPI don;t support this yet",posExpr(term))
                    val mt = MethodType(methodKind)(paramNames)(_ => paramTypes, _ => syncBody.tpe.widen)
                    Lambda(owner, mt,
                       (owner,args) => changeArgs(params,args,syncBody,owner).changeOwner(owner))
                 else
                    term
              case None =>
                 if (existsLambdaUnshift) then
                    body.tpe.widen.asType match
                      case '[Nothing] =>
                        throw MacroError(s"Can't unshift lambda with Nothing type: ${body.tpe.widen.show}", posExpr(term))
                      case '[F[r]] if body.tpe.widen <:< TypeRepr.of[F[r]] =>
                          val nBody = '{ ${ cpsCtx.monad }.flatMap(
                            ${
                              cpsBody.transformed.asExprOf[F[F[r]]]
                             })((x: F[r]) => x) }.asTerm
                          val mt = MethodType(paramNames)(_ => paramTypes, _ => body.tpe.widen)
                          Lambda(owner, mt,
                            (owner, args) => changeArgs(params, args, nBody, owner).changeOwner(owner))
                      case _ =>
                        throw MacroError(s"F[?] expected, we have ${body.tpe.widen.show}",term.asExpr)
                 else
                    throw MacroError(s"Internal error: unshift is called when it not exists",term.asExpr)
         else
            term 

       def extractParamsAndBody(): (List[ValDef], Term) =
         term match
           case Lambda(params, body) => (params, body)
           case _ =>
              throw MacroError(s"Lambda expexted, we have ${term.asExpr.show}",term.asExpr)

       def shiftedArgExpr(existsAsync:Boolean, shiftType: ApplicationShiftType, identType: TypeRepr, params: List[ValDef], body:Term): Term =
         identType match
            case idmt@MethodType(paramNames, paramTypes, resType) =>
                val mt = shiftType match
                  case ApplicationShiftType.CPS_ONLY => cpsShiftedMethodType(paramNames, paramTypes, resType)
                  case ApplicationShiftType.CPS_AWAIT => idmt
                  case ApplicationShiftType.CPS_DEFERR_TO_PLUGIN =>
                    throw MacroError("Internal error: with CPS_DEFERR_TO_PLUGIN we should not call shiftedArgExpr ",term.asExpr)
                createAsyncLambda(mt, params, shiftType, Symbol.spliceOwner)
            case ft@AppliedType(tp,tparams) =>
                  if (ft.isFunctionType) {
                      //val paramTypes = tparams.dropRight(1).map(typeOrBoundsToType(_,false))
                      val paramTypes = tparams.dropRight(1)
                      val resType = tparams.last
                      val paramNames = params.map(_.name)
                      val mt = shiftType match 
                        case ApplicationShiftType.CPS_ONLY => cpsShiftedMethodType(paramNames, paramTypes, resType)
                        case ApplicationShiftType.CPS_AWAIT => 
                           MethodType(paramNames)(_ => paramTypes, _ => resType)
                        case ApplicationShiftType.CPS_DEFERR_TO_PLUGIN =>
                           throw MacroError("Internal error: with CPS_DEFERR_TO_PLUGIN we should not call shiftedArgExpr ",term.asExpr)
                      createAsyncLambda(mt, params, shiftType, Symbol.spliceOwner)
                  } else if (tp <:< partialFunctionType ) {
                      val (tIn, tOut) = tparams match
                         case tIn::tOut::Nil => (tIn, tOut)
                         case _ =>
                           throw MacroError(s"PartialFunction should have 2 type parameters, we have $tparams", term.asExpr)
                      val matchTerm = body match
                         case m@Match(_,_) => m
                         case _ =>
                           throw MacroError(s"PartialFunction should be represented as Match term, we have $body", posExprs(body,term))
                      createAsyncPartialFunction(tIn, tOut, matchTerm, params, shiftType)
                  } else {
                      throw MacroError(s"FunctionType expected, we have ${tp}", term.asExpr)
                  }
            case at@AnnotatedType(underlying,anon) =>
                  shiftedArgExpr(existsAsync, shiftType, underlying, params, body)
            case other =>
                  report.error(
                    s"""
                      MethodType expected, we have ${term.tpe}
                      term.show = ${term.show}
                      term.body = ${term}
                      mt = ${other}
                    """
                  )
                  throw MacroError(s"methodType expected for ${term.asExpr.show}, we have $other",term.asExpr)

       override  def shift(): ApplyArgRecord = copy(optShiftType = Some(ApplicationShiftType.CPS_ONLY))
       override  def withRuntimeAwait(runtimeAwait: Term): ApplyArgRecord =
          copy(optShiftType = Some(ApplicationShiftType.CPS_AWAIT), optRuntimeAwait = Some(runtimeAwait))

       def append(a: CpsTree): CpsTree = 
        report.warning("lambda in statement position",term.pos)
        BlockCpsTree(owner, Queue(term), a.changeOwner(owner))



       private def createAsyncPartialFunction(from: TypeRepr, to: TypeRepr, body: Match, params: List[ValDef], shiftType: ApplicationShiftType): Term =
         val toInF = TypeRepr.of[F].appliedTo(List(to))
         val fromType = from
         val matchVar = body.scrutinee
         val paramNames = params.map(_.name)

         if (cpsCtx.flags.debugLevel >= 15) then
             cpsCtx.log(s"createAsyncPartialFunction: from = $from, to=$to")
 
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
                                 shiftType match
                                   case  ApplicationShiftType.CPS_ONLY =>
                                           termCast[ttt](nTerm)
                                   case  ApplicationShiftType.CPS_AWAIT =>
                                           optRuntimeAwait match
                                             case Some(runtimeAwait) =>
                                               applyRuntimeAwait(runtimeAwait, nTerm, TypeRepr.of[ttt]).asExprOf[ttt]
                                             case None =>
                                               throw MacroError("Internal error: optRuntimeAwait should be defined", posExprs(term))
                                   case  CPS_DEFERR_TO_PLUGIN =>
                                           // it should be the same.
                                           termCast[ttt](nTerm)
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

       private def createAsyncLambda(mt: MethodType, params: List[ValDef], shiftType: ApplicationShiftType,  owner: Symbol): Term =
         val transformedBody = cpsBody.transformed
         val nBody = shiftType match
            case ApplicationShiftType.CPS_ONLY => transformedBody
            case ApplicationShiftType.CPS_AWAIT =>
              optRuntimeAwait match
                case Some(runtimeAwait) =>
                  applyRuntimeAwait(runtimeAwait, transformedBody, mt.resType)
                case None =>
                  throw MacroError("Internal error: optRuntimeAwait should be defined", posExprs(term))
            case ApplicationShiftType.CPS_DEFERR_TO_PLUGIN =>
              throw MacroError("Internal error: with CPS_DEFERR_TO_PLUGIN we should not call createAsyncLambda ",term.asExpr)
         Lambda(owner, mt, (owner,args) => changeArgs(params,args,nBody,owner).changeOwner(owner))

       private def rebindCaseDef(caseDef:CaseDef,
                                 body: Term,
                                 assoc: Map[Symbol, Term],
                                 processBody: Boolean,
                                 owner: Symbol): CaseDef = {


         def rebindPatterns(pattern: Tree, map:Map[Symbol,Term]): (Tree, Map[Symbol, Term]) = {
           pattern match
             case bd@Bind(name,pat1) =>
                val bSym = bd.symbol
                //  -Xcheck-macro complain that bSyn.flags are invalid
                //val nBSym = Symbol.newBind(Symbol.spliceOwner, name, bSym.flags, Ref(bSym).tpe.widen)
                val nBSym = Symbol.newBind(Symbol.spliceOwner, name, Flags.Case, Ref(bSym).tpe.widen)
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
         val nGuard = caseDef.guard.map( TransformUtil.changeSymsInTerm(newBindings, _, owner ) )
         val nBody = if (processBody) TransformUtil.changeSymsInTerm(newBindings, body, owner) else body
         CaseDef(nPattern, nGuard, nBody)
       }

       private def changeArgs(params:List[ValDef], nParams:List[Tree], body: Term, owner: Symbol): Term =
         TransformUtil.substituteLambdaParams(params, nParams, body, owner)


       private def changeIdent(body:Term, oldSym: Symbol, newSym: Symbol, owner: Symbol): Term =
         TransformUtil.changeSymsInTerm(Map(oldSym->Ref(newSym)), body, owner)

  }

  case class ApplyArgNamedRecord(term: NamedArg, name: String, nested: ApplyArgRecord )
     extends ApplyArgRecord {

       def index: Int = nested.index

       def hasShiftedLambda: Boolean = nested.hasShiftedLambda
       def isAsync: Boolean = nested.isAsync
       def noOrderDepended = nested.noOrderDepended
       def shouldBeChangedSync = nested.shouldBeChangedSync
       def isCpsDirect: Boolean = nested.isCpsDirect
       def identArg(existsAsync: Boolean): Term = NamedArg(name, nested.identArg(existsAsync))
       def shift(): ApplyArgRecord = copy(nested=nested.shift())
       def withRuntimeAwait(runtimeAwait: Term): ApplyArgRecord = copy(nested=nested.withRuntimeAwait(runtimeAwait))
       def append(a: CpsTree): CpsTree =
         nested.append(a)

  }



  case class ApplyArgByNameRecord(term: Term,
                                  index: Int,
                                  cpsTree: CpsTree,
                                  optShiftType: Option[ApplicationShiftType],
                                  optRuntimeAwait: Option[Term],
                                 ) extends ApplyArgRecord {

    def identArg(existsAsync: Boolean): Term =
      optShiftType match
        case None => term
        case Some(shiftType) =>
          val rType = shiftType match
            case ApplicationShiftType.CPS_ONLY => TypeRepr.of[F].appliedTo(List(term.tpe.widen))
            case ApplicationShiftType.CPS_AWAIT => term.tpe.widen
            case ApplicationShiftType.CPS_DEFERR_TO_PLUGIN => term.tpe.widen
          val mt = MethodType(List())(_ => List(), _ => rType)
          Lambda(Symbol.spliceOwner,mt, (owner,args) => 
            val transformedBody = cpsTree.transformed
            val nBody = shiftType match
              case ApplicationShiftType.CPS_ONLY => transformedBody
              case ApplicationShiftType.CPS_AWAIT =>
                optRuntimeAwait match
                  case Some(runtimeAwait) =>
                    applyRuntimeAwait(runtimeAwait, transformedBody, rType)
                  case None =>
                    throw MacroError("Internal error: optRuntimeAwait should be defined", posExprs(term))
              case ApplicationShiftType.CPS_DEFERR_TO_PLUGIN => term
            nBody.changeOwner(owner)
          )

    def isAsync: Boolean = false // have shifted-lambda but not async themself cpsTree.isAsync
    def hasShiftedLambda: Boolean = cpsTree.isAsync || optShiftType.isDefined
    def noOrderDepended: Boolean = true
    def isCpsDirect: Boolean = false
    def shouldBeChangedSync: Boolean = cpsTree.isChanged
    override def shift() = copy(optShiftType = Some(ApplicationShiftType.CPS_ONLY))

    override def withRuntimeAwait(runtimeAwait: qctx.reflect.Term): ApplyArgRecord =
      copy(optShiftType = Some(ApplicationShiftType.CPS_AWAIT),  optRuntimeAwait = Some(runtimeAwait))

    override def append(tree: CpsTree): CpsTree =
      throw MacroError("Impossible: preprend in by-name",term.asExpr)
      //tree

  }


  case class ApplyArgInlinedRecord(tree: InlinedCpsTree, nested: ApplyArgRecord )
     extends ApplyArgRecord {
       def index: Int = nested.index
       def term: Term =
              Inlined(tree.origin.call, tree.bindings, nested.term)
       def hasShiftedLambda: Boolean = nested.hasShiftedLambda
       def isAsync: Boolean = nested.isAsync
       def noOrderDepended = nested.noOrderDepended
       def isCpsDirect: Boolean = nested.isCpsDirect
       def shouldBeChangedSync: Boolean = nested.shouldBeChangedSync
       def identArg(existsAsync:Boolean): Term = nested.identArg(existsAsync)
       def shift(): ApplyArgRecord = copy(nested=nested.shift())
       def withRuntimeAwait(runtimeAwait: Term): ApplyArgRecord = copy(nested=nested.withRuntimeAwait(runtimeAwait))
       def append(a: CpsTree): CpsTree =
             val na = nested.append(a)
             if (na eq a)
                a
             else
                InlinedCpsTree(tree.owner, tree.origin, tree.bindings, na)
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


