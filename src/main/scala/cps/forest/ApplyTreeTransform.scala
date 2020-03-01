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
              handleArgs1(applyTerm, cpsObj, args)
          else 
              handleArgs1(applyTerm, CpsTree.pure(fun), args)
        case Ident(name) =>
          handleArgs1(applyTerm, CpsTree.pure(fun), args)
        case _ =>
          val cpsObj = runRoot(obj)  
          handleArgs1(applyTerm, cpsObj, args)
     }


  def handleFunSelect(applyTerm:Term, fun:Term, args:List[Term], obj:Term, method: String): CpsTree = 
     val cpsObj = runRoot(obj)
     if (cpsObj.isAsync) 
        handleArgs1(applyTerm, cpsObj.applyTerm(x => Select(x,fun.symbol), fun.tpe), args)
     else
        handleArgs1(applyTerm, CpsTree.pure(fun), args)
   

  def handleFunIdent(applyTerm: Term, fun:Term, args:List[Term], name: String):CpsTree =
        handleArgs1(applyTerm, CpsTree.pure(fun), args)

  def handleFun(applyTerm: Term, fun:Term, args:List[Term]):CpsTree =
     val cpsFun = runRoot(fun)
     handleArgs1(applyTerm,cpsFun,args)

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
    def identArg: Term
    def isAsync: Boolean
    def noOrderDepended: Boolean
    def useIdent: Boolean = isAsync || !noOrderDepended
    def isOrderDepended = !noOrderDepended
    def append[A: quoted.Type](a: CpsExpr[F,A]): CpsExpr[F,A] 

  case class ApplyArgRepeatRecord(
       term: Repeated,
       elements: List[ApplyArgRecord],
  ) extends ApplyArgRecord {
    override def useIdent: Boolean = (elements.exists(x => x.isAsync || x.isOrderDepended))
    override def identArg: Term = 
      if (useIdent)
          Repeated(elements.map(_.identArg),term.elemtpt)
      else
          term
    override def isAsync = elements.exists(_.isAsync)
    override def noOrderDepended = elements.forall(_.noOrderDepended)
    override def append[A: quoted.Type](a: CpsExpr[F,A]): CpsExpr[F,A] =
       val s0: CpsExpr[F,_] = CpsExpr.unit(cpsCtx.asyncMonad)
       val r = elements.foldLeft(s0){(s,e) => 
           e match
              case e1: ApplyArgTermRecord =>
                  if (e.useIdent) s.append(e1.valDefCpsExpr) else s
              case null =>
                  // impossible: repeated inside repeated
                  throw MacroError("Impossible: repeated inside repeated",cpsCtx.patternCode)
       }
       r.append(a)
  }
  

  case class ApplyArgTermRecord(
       term: Term,
       valDefExpr: Expr[Unit],
       valDefCpsExpr: CpsExpr[F,Unit],
       ident: Ident 
  ) extends ApplyArgRecord
  {
     def isAsync = valDefCpsExpr.isAsync
     def noOrderDepended = termIsNoOrderDepended(term)
     def identArg: Term = 
        if (!isAsync && noOrderDepended)
            term
        else
            ident
     def append[A:quoted.Type](a: CpsExpr[F,A]): CpsExpr[F,A] =
        valDefCpsExpr.append(a)
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
   
  def handleArgs1(applyTerm: Term, cpsFun: CpsTree, 
                      args: List[Term]): CpsTree =  {
        val applyRecords = buildApplyArgsRecords(args, cpsCtx)
        val existsAsyncArgs = applyRecords.exists(_.isAsync)
        if (!existsAsyncArgs) {
           if (!cpsFun.isAsync)
              CpsTree.pure(applyTerm)
           else  
              cpsFun.monadMap(x => Apply(x,args), applyTerm.tpe)
        } else {
           var newArgs = applyRecords.map(_.identArg).toList
           if (haveAsyncLambdaInArgs(args)) 
               // TODO: implement shifted lambda.
               throw MacroError("await inside lambda expressions is not supported yet", cpsCtx.patternCode)
           else 
               val allArgsAreSync = applyRecords.forall(! _.isAsync )
               var runFold = true
               val lastCpsTree = if (allArgsAreSync && cpsFun.isSync) {
                                    runFold = false
                                    CpsTree.pure(applyTerm)
                                 } else {
                                    buildApply(cpsFun, args, applyTerm.tpe)
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
            ApplyArgRepeatRecord(r, buildApplyArgsRecords(rargs, cpsCtx.nestSame("r")) )
         case r@Repeated(rargs, tpt) => 
            ApplyArgRepeatRecord(r, buildApplyArgsRecords(rargs, cpsCtx.nestSame("r")) )
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
                  ApplyArgTermRecord(t,unitBlockExpr,
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

  def buildApply(cpsFun: CpsTree, args: List[Term], applyTpe: Type) = 
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


