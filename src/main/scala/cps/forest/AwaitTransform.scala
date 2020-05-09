package cps.forest

import scala.quoted._

import cps._
 
object AwaitTransform:

  /**
   *'''
   * '{ _root_.cps.await[F,$ftType]($ft) } 
   *'''
   **/
  def apply[F[_]:Type,T:Type,S:Type](cpsCtx: TransformationContext[F,T], 
                                    fType:  Type[S], ft:Expr[F[S]])(
                                        using qctx: QuoteContext): CpsExpr[F,T] =
     import qctx.tasty.{_, given _}
     import util._
     import cpsCtx._
     if (cpsCtx.flags.debugLevel >= 10)
        println(s"AwaitTransform, ft=${ft.show}")
     val awBuild = CpsExpr.async[F,S](monad, ft)
     //TODO: get data about independence and rewrite.
     awBuild.asInstanceOf[CpsExpr[F,T]]
     

