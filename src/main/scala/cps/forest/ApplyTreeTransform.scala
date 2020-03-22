package cps.forest

import scala.quoted._
import scala.quoted.matching._

import cps._
import cps.misc._


trait ApplyTreeTransform[F[_]]:

  thisTreeTransform: TreeTransformScope[F] =>
  
  import qctx.tasty.{_, given _}


  // case Apply(fun,args) 
  def runApply(applyTerm: Term, 
              fun: Term, 
              args: List[Term]): CpsTree =
     if (cpsCtx.flags.debugLevel >= 10)
       println(s"runApply, appyTerm=${applyTerm}")
     val monad = cpsCtx.asyncMonad
     // try to omit things, which should be eta-expanded,
     fun match 
       case TypeApply(obj,targs) =>
          // check - maybe this is await
          obj match {
            case Ident(name) if (name=="await") =>
                   if ( obj.symbol == awaitSymbol ) 
                     if (targs.head.tpe =:= monadTypeTree.tpe) 
                         // TODO: check, that args have one element
                        val awaitArg = args.head
                        runAwait(applyTerm, awaitArg)
                     else
                        // not my await [?]
                        if (cpsCtx.flags.debugLevel >= 10)
                           println("Not-my-await") 
                        // ??  TODO: apply conversion if exists or touch unchanged
                        handleFunTypeApply(applyTerm,fun,args,obj,targs)
                   else
                     handleFunTypeApply(applyTerm,fun,args,obj,targs)
            case Select(obj1, name) if (name=="await") =>
                   if ( obj1.symbol == awaitSymbol ) 
                     if (targs.head.tpe =:= monadTypeTree.tpe) 
                        runAwait(applyTerm, args.head)
                     else
                        handleFunTypeApply(applyTerm,fun,args,obj,targs)
                   else
                     handleFunTypeApply(applyTerm,fun,args,obj,targs)
            case _ => handleFunTypeApply(applyTerm, fun, args, obj, targs)
          }
       case Select(obj,method) =>
            handleFunSelect(applyTerm, fun, args, obj, method)
       case Ident(name) =>
            handleFunIdent(applyTerm, fun, args, name)
       case _ =>
            handleFun(applyTerm, fun, args)

      
  def handleFunTypeApply(applyTerm: Term, 
                         fun:Term, 
                         args: List[Term], 
                         obj:Term, targs:List[TypeTree]): CpsTree =
     obj match {
        case Select(obj1,method) =>
          val cpsObj1 = runRoot(obj1)
          if (cpsObj1.isAsync) 
              val cpsObj = cpsObj1.applyTerm(x => TypeApply(Select(x,obj1.symbol),targs), fun.tpe)
              handleArgs1(applyTerm, fun, cpsObj, args)
          else 
              handleArgs1(applyTerm, fun, CpsTree.pure(fun), args)
        case Ident(name) =>
          handleArgs1(applyTerm, fun, CpsTree.pure(fun), args)
        case _ =>
          val cpsObj = runRoot(obj)  
          handleArgs1(applyTerm, fun, cpsObj, args)
     }


  def handleFunSelect(applyTerm:Term, fun:Term, args:List[Term], obj:Term, method: String): CpsTree = 
     val cpsObj = runRoot(obj)
     if (cpsObj.isAsync) 
        handleArgs1(applyTerm, fun, cpsObj.applyTerm(x => Select(x,fun.symbol), fun.tpe), args)
     else
        handleArgs1(applyTerm, fun, CpsTree.pure(fun), args)
   

  def handleFunIdent(applyTerm: Term, fun:Term, args:List[Term], name: String):CpsTree =
        handleArgs1(applyTerm, fun, CpsTree.pure(fun), args)

  def handleFun(applyTerm: Term, fun:Term, args:List[Term]):CpsTree =
     val cpsFun = runRoot(fun)
     handleArgs1(applyTerm, fun, cpsFun, args)

  /**
   * How to handle arguments?
   *  We want keep evaluation order from left to right, so, imagine we have 
   *  function f(a1,a2,a3) and a2 is async, a1, a3 - sync.
   *  we will transform this to
   *  ```
   *  { val arg1 = a1; 
   *    transform(a2).flatMap( x => 
   *       { val arg2 = x; 
   *         val arg3 = a3;  
   *         f(arg1, arg2, arg3) 
   *  }    }
   *  ```
   *
   * more generally we at first generate block:
   *   { arg1 = a1; .....  argN = aN; f(arg1,...argN) }
   * and then transform one, knowing that all arguments to f are sync
   *  (of course, if all arguments are sync, we just call f(arg1,... arg2) without all this machinery)
   *
   * And we don't want to generate tree and then resu 
   *
  **/

  sealed trait ApplyArgRecord:
    def term: Term
    def index: Int
    def identArg: Term
    def isAsync: Boolean
    def hasShiftedLambda: Boolean
    def noOrderDepended: Boolean
    def useIdent: Boolean = isAsync || !noOrderDepended
    def isOrderDepended = !noOrderDepended
    def shift(): ApplyArgRecord
    def append[A: quoted.Type](a: CpsExpr[F,A]): CpsExpr[F,A] 

  case class ApplyArgRepeatRecord(
       term: Repeated,
       index: Int,
       elements: List[ApplyArgRecord],
  ) extends ApplyArgRecord {
    override def useIdent: Boolean = (elements.exists(x => x.isAsync || x.isOrderDepended))
    override def identArg: Term = 
      if (useIdent)
          Repeated(elements.map(_.identArg),term.elemtpt)
      else if (hasShiftedLambda)
          Repeated(elements.map(_.identArg),shiftedLambdaTypeTree(term.elemtpt))
      else
          term
    override def isAsync = elements.exists(_.isAsync)
    override def hasShiftedLambda = elements.exists(_.hasShiftedLambda)
    override def noOrderDepended = elements.forall(_.noOrderDepended)
    override def shift() = copy(elements = elements.map(_.shift()))
    override def append[A: quoted.Type](a: CpsExpr[F,A]): CpsExpr[F,A] =
       val s0: CpsExpr[F,_] = CpsExpr.unit(cpsCtx.asyncMonad)
       val r = elements.foldLeft(s0){(s,e) => 
           e match
              case e1: ApplyArgTermRecord =>
                  if (e.useIdent) s.append(e1.valDefCpsExpr) else s
              case _: ApplyArgLambdaRecord => s
              case null =>
                  // impossible: repeated inside repeated
                  throw MacroError("Impossible: repeated inside repeated",cpsCtx.patternCode)
       }
       r.append(a)
  }
  

  case class ApplyArgTermRecord(
       term: Term,
       index: Int,
       valDefExpr: Expr[Unit],
       valDefCpsExpr: CpsExpr[F,Unit],
       ident: Ident 
  ) extends ApplyArgRecord
  {
     def isAsync = valDefCpsExpr.isAsync
     def hasShiftedLambda: Boolean = false
     def noOrderDepended = termIsNoOrderDepended(term)
     def identArg: Term = 
        if (!isAsync && noOrderDepended)
            term
        else
            ident
     def shift(): ApplyArgRecord = this
     def append[A:quoted.Type](a: CpsExpr[F,A]): CpsExpr[F,A] =
        valDefCpsExpr.append(a)
  }

  case class ApplyArgLambdaRecord(
       term: Block,   // Lambda,  see coding of Lambda in Tasty Reflect.
       index: Int,
       cpsBody: CpsTree,
       shifted: Boolean
  ) extends ApplyArgRecord {

       def hasShiftedLambda: Boolean = cpsBody.isAsync

       def isAsync: Boolean = false

       def noOrderDepended: Boolean = true

       def identArg: Term = 
         if (hasShiftedLambda || shifted) 
            val params = term match
              case Lambda(params, body) => params
              case _ =>
                 throw MacroError(s"Lambda expexted, we have ${term.seal.show}",term.seal)
            val mt = term.tpe match 
              case MethodType(paramNames, paramTypes, resType) =>
                  shiftedMethodType(paramNames, paramTypes, resType)
              case ft@AppliedType(tp,tparams) =>
                  if (ft.isFunctionType) {
                      println("IsFunctionType!!!")
                      val paramTypes = tparams.dropRight(1).map(typeOrBoundsToType(_,false)) 
                      val resType = typeOrBoundsToType(tparams.last,true)
                      val paramNames = params.map(_.name)
                      shiftedMethodType(paramNames, paramTypes, resType)
                  } else {
                      throw MacroError(s"FunctionType expected, we have ${tp}", term.seal) 
                  }
              case other =>
                  // TODO: logging compiler interface instead println
                  println(s"MethodType expected, we have ${term.tpe}") 
                  println(s"term.show = ${term.show}") 
                  println(s"term.body = ${term}") 
                  println(s"mt = ${other}") 
                  throw MacroError(s"methodType expected for ${term.seal.show}, we have $other",term.seal)
            Lambda(mt, args => changeArgs(params,args,cpsBody.transformed))
         else 
            term

       def shift(): ApplyArgRecord = copy(shifted=true)

       def append[A: quoted.Type](a: CpsExpr[F,A]): CpsExpr[F,A] = a

       private def changeArgs(params:List[ValDef], nParams:List[Tree], body: Term): Term =
         val association: Map[Symbol, Tree] = (params zip nParams).foldLeft(Map.empty){
           case (m, (oldParam, newParam)) => m.updated(oldParam.symbol, newParam)
         }
         val changes = new TreeMap() {
             override def transformTerm(tree:Term)(using ctx: Context):Term =
               tree match
                 case ident@Ident(name) => association.get(ident.symbol) match
                                            case Some(paramTree) =>
                                              paramTree match 
                                                case paramTerm: Term => paramTerm
                                                case _ =>
                                                 throw MacroError(s"term expected for lambda param, we ahave ${paramTree}",term.seal)
                                            case None => super.transformTerm(tree)
                 case _ => super.transformTerm(tree)
         }
         changes.transformTerm(body)
         
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

  def typeOrBoundsToType(x: TypeOrBounds, isHight: Boolean = true): Type =
    x match 
      case y: Type => y
      case TypeBounds(low,hight) => if (isHight) hight else low
      case NoPrefix => if (isHight) defn.AnyType else defn.NothingType

  def shiftedLambdaTypeTree(tpt: TypeTree): TypeTree =
    Inferred(shiftedLambdaType(tpt.tpe))

  def shiftedLambdaType(tpe: Type): Type =
    tpe match {
      case MethodType(paramNames, paramTypes, resType) =>
               // currently no support for path-dependend lambdas.
               MethodType(paramNames)( mt => paramTypes, 
                                       mt => AppliedType(fType.unseal.tpe, List(resType)))
      case PolyType(paramNames,paramBounds,resType) =>
               PolyType(paramNames)(pt => paramBounds,
                                    pt => AppliedType(fType.unseal.tpe, List(resType)))
      case _ => throw MacroError("Not supported type for shifting: ${tpe}",cpsCtx.patternCode)
    }
   
  def handleArgs1(applyTerm: Term, fun: Term, cpsFun: CpsTree, 
                      args: List[Term]): CpsTree =  {
        val applyRecords = buildApplyArgsRecords(args, cpsCtx)
        val existsAsyncArgs = applyRecords.exists(_.isAsync)
        val shiftedLambdaIndexes = applyRecords.filter(_.hasShiftedLambda).map(_.index).toSet
        if (!existsAsyncArgs && shiftedLambdaIndexes.isEmpty) {
           if (!cpsFun.isAsync)
              CpsTree.pure(applyTerm)
           else  
              cpsFun.monadMap(x => Apply(x,args), applyTerm.tpe)
        } else {
           val newArgs = applyRecords.map{ a =>
                 if (!shiftedLambdaIndexes.isEmpty) 
                    a.shift()
                 else 
                    a
                }.map(_.identArg)
           val allArgsAreSync = applyRecords.forall(! _.isAsync )
           var runFold = true
           val lastCpsTree: CpsTree = if (allArgsAreSync && cpsFun.isSync) {
                                    runFold = false
                                    if (shiftedLambdaIndexes.isEmpty) {
                                       CpsTree.pure(applyTerm)
                                    } else {
                                       buildApply(cpsFun, fun, newArgs, applyTerm.tpe, shiftedLambdaIndexes)
                                    }
                                 } else {
                                    buildApply(cpsFun, fun, newArgs, applyTerm.tpe, shiftedLambdaIndexes)
                                 }
               if (runFold) 
                 applyTerm.seal match
                   case '{ $t: $tt } =>
                      val lastCpsExpr = lastCpsTree.toResult(t)
                      val expr = applyRecords.foldRight(lastCpsExpr){ (p,s) =>
                             if (p.useIdent) 
                                 p.append(s)
                             else
                                 s
                      }
                      exprToTree(expr,applyTerm)
                   case _ => 
                       throw MacroError(s"Can't determinate type for $applyTerm",applyTerm.seal)
               else
                 lastCpsTree
        }
  }

  def buildApplyArgsRecords(args: List[Term], cpsCtx:TransformationContext[F,?]): List[ApplyArgRecord] = {
     import scala.internal.quoted.showName
     import scala.quoted.QuoteContext
     import scala.quoted.Expr
     args.zipWithIndex.map{ (t:Term, i:Int) =>
       t match {
         case tr@Typed(r@Repeated(rargs, tpt),tpt1) => 
            ApplyArgRepeatRecord(r, i, buildApplyArgsRecords(rargs, cpsCtx.nestSame("r")) )
         case r@Repeated(rargs, tpt) => 
            ApplyArgRepeatRecord(r, i, buildApplyArgsRecords(rargs, cpsCtx.nestSame("r")) )
         case lambda@Lambda(params, body) => 
            val cpsBody = runRoot(body)
            ApplyArgLambdaRecord(lambda,i,cpsBody, false)
         case _ =>
            t.seal match {
              case '{ $x:$tx } =>
                  val argName:String = "a"+i
                  val valDefExpr = '{
                                      @showName(${Expr(argName)})
                                      val a:${tx} = ${x}
                                      a
                                    }
                  val valDef = TransformUtil.find(valDefExpr.unseal, {
                        case v@ValDef(_,_,_) => Some(v)
                        case _ => None
                  }).get.asInstanceOf[ValDef]
                  val ident = TransformUtil.find(valDefExpr.unseal, {
                        case id@Ident(name) => if (id.symbol == valDef.symbol) 
                                                  Some(id)
                                               else None
                        case _ => None
                  }).get.asInstanceOf[Ident]
                  val unitBlockExpr = Block( valDef::Nil, Literal(Constant(())) ).seal.
                                                                     asInstanceOf[Expr[Unit]]
                  val valDefCpsExpr = ValDefTransform.fromBlock(
                                        TransformationContext(
                                          unitBlockExpr,
                                          quoted.Type.UnitTag,
                                          cpsCtx.asyncMonad,
                                          cpsCtx.flags,
                                          cpsCtx.exprMarker + argName, 
                                          cpsCtx.nesting + 1
                                        ), 
                                        valDef
                                      )
                  if (cpsCtx.flags.debugLevel > 15) {
                       println(s"buildApplyArg, t=$t")
                       println(s"buildApplyArg, unitBlockExpr=${unitBlockExpr.show}")
                       println(s"buildApplyArg, valDefCpsExpr=${valDefCpsExpr}")
                  }
                  ApplyArgTermRecord(t,i, unitBlockExpr,
                                     valDefCpsExpr, ident)
           }
       }
    }
  }

  def haveAsyncLambdaInArgs(args:List[Term]):Boolean = 
     args.exists{ x =>
       x match
          case Lambda(largs,body) => TransformUtil.containsAwait(body)
          case Repeated(rargs,tpt) => haveAsyncLambdaInArgs(rargs)
          case _ => false
     }

  def findAsyncShift[E:quoted.Type](e:Expr[E]):Option[Expr[AsyncShift[E]]] =
    Expr.summon[AsyncShift[E]]
      

  def shiftedApply(term: Term, args: List[Term], shiftedIndexes:Set[Int]): Term =

    def shiftQual(x:Term):Term = 
       x.seal match 
         case '{ $e: $et} =>
            findAsyncShift(e) match
              case Some(shifted) => shifted.unseal
              case None => 
                   // TODO: provide some other alternatives ?
                   throw MacroError(s"Can't find AsyncShift for ${x.tpe.seal.show}",x.seal)
         case _ =>
            throw MacroError(s"Can't find AsyncShift for ${x}",x.seal)

    //TODO: will work only for unique.
    //   change dotty API to find case, where name is not unique
    //    in qualifier
    def shiftSelect(x:Select):Select = 
          val sq = shiftQual(x.qualifier)
          println("sq=$sq")
          println("name=$x.name")
          Select.unique(shiftQual(x.qualifier),x.name)
       
    def shiftCaller(term:Term): Term = 
       val monad = cpsCtx.asyncMonad.unseal
       term match
          case TypeApply(s@Select(qual,name),targs) =>
                  val newSelect = shiftSelect(s)
                  TypeApply(newSelect, fType.unseal::targs).appliedTo(qual,monad)
          case s@Select(qual,name) =>
                  TypeApply(shiftSelect(s), fType.unseal::Nil).appliedTo(qual, monad)
          case TypeApply(x, targs) => 
                  TypeApply(shiftCaller(x),targs)
          case Lambda(params, body) =>
                  val shiftedSymbols = params.zipWithIndex.filter{ 
                      (p,i) => shiftedIndexes.contains(i)
                  }.map{ (p,i) => p.symbol }.toSet 
                  val nBody = asyncShift(body, shiftedSymbols)
                  ???
          case Block(statements, last) =>
                  Block(statements, shiftCaller(last))
          case _ => 
                  // TODO: check, that we can seal term
                  throw MacroError("Can't shift caller ${term}",term.seal)

    Apply(shiftCaller(term),args)

  end shiftedApply

  def buildApply(cpsFun: CpsTree, fun: Term, args: List[Term], applyTpe: Type, shiftedIndexes: Set[Int]): CpsTree = 
        if (!shiftedIndexes.isEmpty)
          if (cpsFun.isSync) 
            CpsTree.impure(shiftedApply(fun, args, shiftedIndexes), applyTpe)
          else
            cpsFun.monadFlatMap(x => shiftedApply(x,args, shiftedIndexes),applyTpe) 
        else
          if (cpsFun.isSync) 
            cpsFun.applyTerm(_.appliedToArgs(args), applyTpe)
          else
            cpsFun.monadMap(x => Apply(x,args), applyTpe)

     
object ApplyTreeTransform:


  def run[F[_]:Type,T:Type](using qctx: QuoteContext)(cpsCtx: TransformationContext[F,T],
                         applyTerm: qctx.tasty.Term,
                         fun: qctx.tasty.Term,
                         args: List[qctx.tasty.Term]): CpsExpr[F,T] = {
     //val tmpCpsCtx = cpsCtx
     val tmpQctx = qctx
     val tmpFtype = summon[Type[F]]
     class Bridge(tc:TransformationContext[F,T]) extends TreeTransformScope[F]
                                                    with TreeTransformScopeInstance[F,T](tc) {

         implicit val fType = tmpFtype

         def bridge(): CpsExpr[F,T] =
            runApply(applyTerm.asInstanceOf[qctx.tasty.Term],
                         fun.asInstanceOf[qctx.tasty.Term],
                         args.asInstanceOf[List[qctx.tasty.Term]]
                        ).toResult(cpsCtx.patternCode).asInstanceOf[CpsExpr[F,T]]

     }
     (new Bridge(cpsCtx)).bridge()
  }


