package cps.forest

import scala.quoted._

import cps._
 
object IfTransform:

  /**
   *'''
   * '{ if ($cond)  $ifTrue  else $ifFalse } 
   *'''
   **/
  def run[F[_]:Type,T:Type](cpsCtx: TransformationContext[F,T], 
                               cond: Expr[Boolean], ifTrue: Expr[T], ifFalse: Expr[T]
                               )(using Quotes): CpsExpr[F,T] =
     import quotes.reflect._
     import util._
     import cpsCtx._
     val cR = Async.nestTransform(cond, cpsCtx, TransformationContextMarker.IfCond)
     val tR = Async.nestTransform(ifTrue, cpsCtx, TransformationContextMarker.IfTrue)
     val fR = Async.nestTransform(ifFalse, cpsCtx, TransformationContextMarker.IfFalse)
     var isAsync = true

     val cnBuild = {
       if (!cR.isAsync)
         if (!tR.isAsync && !fR.isAsync) 
            isAsync = false
            CpsExpr.sync(monad, patternCode, cR.isChanged || tR.isChanged || fR.isChanged)
         else
            CpsExpr.async[F,T](monad,
                '{ if ($cond) 
                     ${tR.transformed}
                   else 
                     ${fR.transformed} })
       else // (cR.isAsync) 
         def condAsyncExpr() = cR.transformed
         if (!tR.isAsync && !fR.isAsync) 
           CpsExpr.async[F,T](monad,
                    '{ ${monad}.map(
                                 ${condAsyncExpr()}
                        )( c =>
                                   if (c) {
                                     ${ifTrue}
                                   } else {
                                     ${ifFalse}
                                   } 
                     )})
         else
           CpsExpr.async[F,T](monad,
                   '{ ${monad}.flatMap(
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
     

