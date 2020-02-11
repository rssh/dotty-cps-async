package cps.forest

import scala.quoted._
import scala.quoted.matching._

import cps._
import cps.misc._


trait ApplyTreeTransform[F[_]]:

  thisTreeTransform: TreeTransformScope[F] =>
  
  import qctx.tasty.{_, given}


  // case Apply(fun,args) 
  def runApply(applyTerm: Term, 
              fun: Term, 
              args: List[Term]): CpsTree =
     val monad = cpsCtx.asyncMonad
     // try to omit things, which should be eta-expanded,
     fun match 
       case TypeApply(obj,targs) =>
          // check - maybe this is await
          obj match {
            case Ident(name) if (name=="await") =>
                   println(s"await detected, obj.tpe=${obj.tpe}")
                   println(s"obj.symbol=${obj.symbol}")
                   println(s"targs = ${targs}")
                   println(s"targs.length = ${targs.length}")
                   if ( obj.symbol == awaitSymbol ) 
                     if (targs.head.tpe =:= monadTypeTree.tpe) 
                         // TODO: check, that args have one element
                        val awaitArg = args.head
                        runAwait(applyTerm, awaitArg)
                     else
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
              handleArgs(applyTerm, cpsObj, args)
          else 
              handleArgs(applyTerm, CpsTree.pure(fun), args)
        case Ident(name) =>
          handleArgs(applyTerm, CpsTree.pure(fun), args)
        case _ =>
          val cpsObj = runRoot(obj)  
          handleArgs(applyTerm, cpsObj, args)
     }


  def handleFunSelect(applyTerm:Term, fun:Term, args:List[Term], obj:Term, method: String): CpsTree = 
     val cpsObj = runRoot(obj)
     if (cpsObj.isAsync) 
        handleArgs(applyTerm, cpsObj.applyTerm(x => Select(x,fun.symbol), fun.tpe), args)
     else
        handleArgs(applyTerm, CpsTree.pure(fun), args)
   

  def handleFunIdent(applyTerm: Term, fun:Term, args:List[Term], name: String):CpsTree =
        handleArgs(applyTerm, CpsTree.pure(fun), args)

  def handleFun(applyTerm: Term, fun:Term, args:List[Term]):CpsTree =
     val cpsFun = runRoot(fun)
     handleArgs(applyTerm,cpsFun,args)

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
  trait ApplyArgGenerator {
     def isConst: Boolean
     def isAsync: Boolean
     def argName: String
  }
   

  def handleArgs(applyTerm: Term, cpsFun: CpsTree, args: List[Term]): CpsTree = 
        // TODO: maybe handle repeated separately ??
 
        

        val cpsArgs = args.map(x => runRoot(x))
        val isArgsAsync = cpsArgs.exists(_.isAsync)
        val isAsync = cpsFun.isAsync || isArgsAsync
        if (!isAsync) 
           CpsTree.pure(applyTerm)
        else if (cpsFun.isAsync && !isArgsAsync) 
           cpsFun.monadMap(x => Apply(x,args), applyTerm.tpe)
        else 
           throw MacroError("await inside args is not supported yet",cpsCtx.patternCode)
        
     
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


