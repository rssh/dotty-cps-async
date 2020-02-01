// CPS Transform for tasty apply
// (C) Ruslan Shevchenko <ruslan@shevchenko.kiev.ua>, 2019
package cps.forest

import scala.quoted._
import scala.quoted.matching._

import cps._
import cps.misc._


class ApplyTransform[F[_]:Type,T:Type](cpsCtx: TransformationContext[F,T])

  import cpsCtx._

  // case Apply(fun,args) 
  def run(given qctx: QuoteContext)(fun: qctx.tasty.Term, args: List[qctx.tasty.Term]): CpsExpr[F,T] =
     import qctx.tasty.{_, given}
     println(s"!!! apply detected : ${fun} ${args}")
     fun match 
       case Select(obj,method) =>
         obj.seal match
           case '{ $e: $et } =>
                     val r = Async.rootTransform(e, asyncMonad, false)
                     if (r.isAsync)
                         handleArgs(???  , args)
                     else
                         handleArgs( r, args)

  def handleArgs[X:Type](given qctx: QuoteContext)(cpsFun: CpsExpr[F,X], args: List[qctx.tasty.Term]): CpsExpr[F,T] = 
        import qctx.tasty.{_, given}
        val cpsArgs = args.map{ x =>
                x.seal match 
                 case '{ $e: $et } =>
                     Async.rootTransform(e, asyncMonad, false)
             }
        val isArgsAsync = cpsArgs.exists(_.isAsync)
        val isAsync = cpsFun.isAsync || isArgsAsync
        if (!isAsync) 
           CpsExpr.sync(asyncMonad, patternCode)
        else if (cpsFun.isAsync && !isArgsAsync) 
           CpsExpr.async(asyncMonad, '{ ${asyncMonad}.map(${cpsFun.transformed})(
                                            a => ${Apply('a.unseal,args).seal}) }.asInstanceOf[Expr[F[T]]] )
        else 
           throw MacroError("await inside args is not supported yet",cpsCtx.patternCode)



