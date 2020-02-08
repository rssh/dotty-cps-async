package cps.forest

import scala.quoted._
import scala.quoted.matching._

import cps._
 
object ThrowTransform

  /**
   *'''
   * '{ throw $ex } 
   *'''
   **/
  def run[F[_]:Type,T:Type, S<:Throwable:Type](cpsCtx: TransformationContext[F,T], 
                               ex: Expr[S]
                               )(given qctx: QuoteContext): CpsExpr[F,T] =
     import qctx.tasty.{_, given}
     import util._
     import cpsCtx._
     val cpsEx = Async.rootTransform(ex, asyncMonad, exprMarker+"E")

     if (!cpsEx.isAsync)
            // TODO: think, mb leave as is...
            CpsExpr.async[F,T](asyncMonad,
                                          '{  ${asyncMonad}.error(${ex}) })
     else  
            CpsExpr.async[F,T](asyncMonad,
                cpsEx.flatMap[T]( '{ (ex:S) => ${asyncMonad}.error(ex) } ).transformed )
     

