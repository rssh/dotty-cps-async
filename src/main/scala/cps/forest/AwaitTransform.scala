package cps.forest

import scala.quoted._
import scala.quoted.matching._

import cps._
 
object AwaitTransform:

  /**
   *'''
   * '{ _root_.cps.await[F,$ftType]($ft) } 
   *'''
   **/
  def apply[F[_]:Type,T:Type,S:Type](transformationContext: TransformationContext[F,T], 
                                    fType:  Type[S], ft:Expr[F[S]])(
                                        using qctx: QuoteContext): CpsExpr[F,T] =
     import qctx.tasty.{_, given _}
     import util._
     import transformationContext._
     // TODO: think about situation, when ftType != tType.
     // [independend / translated ]
     val awBuild = CpsExpr.async[F,S](asyncMonad, ft)
     //TODO: get data about independence and rewrite.
     awBuild.asInstanceOf[CpsExpr[F,T]]
     

