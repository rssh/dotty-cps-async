package cps.forest

import scala.quoted._
import scala.quoted.matching._

import cps._
 
object AwaitTransform

  /**
   *'''
   * '{ _root_.cps.await[F,$ftType]($ft) } 
   *'''
   **/
  def apply[F[_]:Type,T:Type,S:Type](transformationContext: TransformationContext[F,T], 
                                    fType:  Type[S], ft:Expr[F[S]])(
                                        given qctx: QuoteContext): CpsExprResult[F,T] =
     import qctx.tasty.{_, given}
     import util._
     import transformationContext._
     // TODO: think about situation, when ftType != tType.
     // [independend / translated ]
     val awBuild = new CpsChunkBuilder[F,S] {
           override def create() = CpsChunk(Seq(),ft)
           override def append[A:quoted.Type](e:CpsChunk[F,A]) = 
                           CpsChunk[F,A](Seq(),
                             '{
                               ${asyncMonad}.flatMap($ft)(_ => ${e.toExpr})
                              }
                           )
     }
     //TODO: get data about independence and rewrite.
     val awBuildCasted = awBuild.asInstanceOf[CpsChunkBuilder[F,T]]
     CpsExprResult[F,T](patternCode, awBuildCasted, patternType, true)
     

