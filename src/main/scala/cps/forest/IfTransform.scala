package cps.forest

import scala.quoted._
import scala.quoted.matching._

import cps._
 
object IfTransform

  /**
   *'''
   * '{ _root_.cps.await[F,$ftType]($ft) } 
   *'''
   **/
  def run[F[_]:Type,T:Type](cpsCtx: TransformationContext[F,T], 
                               cond: Expr[Boolean], ifTrue: Expr[T], ifFalse: Expr[T]
                               )(given qctx: QuoteContext): CpsExprResult[F,T] =
     import qctx.tasty.{_, given}
     import util._
     import cpsCtx._
     val cR = Async.rootTransform(cond, asyncMonad)
     val tR = Async.rootTransform(ifTrue, asyncMonad)
     val fR = Async.rootTransform(ifFalse, asyncMonad)
     var isAsync = true

     val cnBuild = {
       if (!cR.haveAwait)
         if (!tR.haveAwait && !fR.haveAwait) 
            isAsync = false
            CpsChunkBuilder.sync(patternCode,asyncMonad)
         else
            new CpsChunkBuilder[F,T] {
               override def create() = fromFExpr(
                '{ if ($cond) 
                     ${tR.cpsBuild.create().toExpr}
                   else 
                     ${fR.cpsBuild.create().toExpr} })
               override def append[A:quoted.Type](e:CpsChunk[F,A]) =
                   CpsChunk[F,A](Seq(), '{
                       ${asyncMonad}.flatMap(
                          ${create().toExpr})( _ =>
                                          ${e.toExpr})
                    })
            }
       else // (cR.haveAwait) 
         def condAsyncExpr() = cR.cpsBuild.create().toExpr
         if (!tR.haveAwait && !fR.haveAwait) 
           new CpsChunkBuilder[F,T] {
             val mappedCond = '{ ${asyncMonad}.map(
                                 ${condAsyncExpr()}
                                )( c =>
                                   if (c) {
                                     ${ifTrue}
                                   } else {
                                     ${ifFalse}
                                   } 
                                )} 

              override def create() = fromFExpr(mappedCond)

              override def append[A:quoted.Type](e:CpsChunk[F,A]) =
                       CpsChunk[F,A](Seq(),
                          '{ ${asyncMonad}.flatMap(
                                ${mappedCond}
                               )(_ => ${e.toExpr})
                           })
 
           }
         else
           new CpsChunkBuilder[F,T]{
              override def create() = fromFExpr(
                   '{ ${asyncMonad}.flatMap(
                         ${condAsyncExpr()}
                       )( c =>
                           if (c) {
                              ${tR.cpsBuild.create().toExpr}
                           } else {
                              ${fR.cpsBuild.create().toExpr} 
                           } 
                        )
                    }) 
              override def append[A:quoted.Type](e:CpsChunk[F,A]) = 
                   CpsChunk[F,A](Seq(),
                        '{  ${asyncMonad}.flatMap(
                               ${create().toExpr})(_ => ${e.toExpr}) 
                        })
           }
     }
     CpsExprResult[F,T](patternCode, cnBuild, patternType, isAsync)
     

