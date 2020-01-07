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
                               )(given qctx: QuoteContext): CpsExprResult[F,T] =
     import qctx.tasty.{_, given}
     import util._
     import cpsCtx._
     val cpsEx = Async.rootTransform(ex, asyncMonad, false)

     val cnBuild = {
       if (!cpsEx.haveAwait)
            new CpsChunkBuilder[F,T](asyncMonad) {
               override def create() = fromFExpr(
                '{  ${asyncMonad}.error(${ex}) })
               override def append[A:quoted.Type](e:CpsChunk[F,A]) =
                     flatMapIgnore(e.toExpr)
            }
       else  
            new CpsChunkBuilder[F,T](asyncMonad) {
              override def create() = 
                cpsEx.chunkBuilder.flatMap( '{ (ex:S) => ${asyncMonad}.error(ex) } )
              override def append[A:quoted.Type](e:CpsChunk[F,A]) =
                     flatMapIgnore(e.toExpr)
            }
     }
     CpsExprResult[F,T](patternCode, cnBuild, patternType, true)
     

