package cps.forest

import scala.quoted._
import scala.quoted.matching._

import cps._
 
object IfTransform

  /**
   *'''
   * '{ if ($cond)  $ifTrue  else $ifFalse } 
   *'''
   **/
  def run[F[_]:Type,T:Type](cpsCtx: TransformationContext[F,T], 
                               cond: Expr[Boolean], ifTrue: Expr[T], ifFalse: Expr[T]
                               )(given qctx: QuoteContext): CpsExpr[F,T] =
     import qctx.tasty.{_, given}
     import util._
     import cpsCtx._
     val cR = Async.rootTransform(cond, asyncMonad, false)
     val tR = Async.rootTransform(ifTrue, asyncMonad, false)
     val fR = Async.rootTransform(ifFalse, asyncMonad, false)
     var isAsync = true

     val cnBuild = {
       if (!cR.isAsync)
         if (!tR.isAsync && !fR.isAsync) 
            isAsync = false
            CpsExpr.sync(asyncMonad, patternCode)
         else
            CpsExpr.async[F,T](asyncMonad,
                '{ if ($cond) 
                     ${tR.transformed}
                   else 
                     ${fR.transformed} })
       else // (cR.isAsync) 
         def condAsyncExpr() = cR.transformed
         if (!tR.isAsync && !fR.isAsync) 
           CpsExpr.async[F,T](asyncMonad,
                    '{ ${asyncMonad}.map(
                                 ${condAsyncExpr()}
                        )( c =>
                                   if (c) {
                                     ${ifTrue}
                                   } else {
                                     ${ifFalse}
                                   } 
                     )})
         else
           CpsExpr.async[F,T](asyncMonad,
                   '{ ${asyncMonad}.flatMap(
                         ${condAsyncExpr()}
                       )( c =>
                           if (c) {
                              ${tR.transformed}
                           } else {
                              ${fR.transformed} 
                           } 
                        )
                    }) 
       }
     cnBuild
     

