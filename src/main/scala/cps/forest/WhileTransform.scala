package cps.forest

import scala.quoted._
import scala.quoted.matching._

import cps._
 
object WhileTransform

  /**
   *'''
   * '{ _root_.cps.await[F,$ftType]($ft) } 
   *'''
   **/
  def run[F[_]:Type,T:Type](cpsCtx: TransformationContext[F,T], 
                               cond: Expr[Boolean], repeat: Expr[Unit]
                               )(given qctx: QuoteContext): CpsExprResult[F,T] =
     import qctx.tasty.{_, given}
     import util._
     import cpsCtx._
     val cpsCond = Async.rootTransform(cond, asyncMonad, false)
     val cpsRepeat = Async.rootTransform(repeat, asyncMonad, false)
     val isAsync = cpsCond.haveAwait || cpsRepeat.haveAwait

     val builder = {
       if (!cpsCond.haveAwait)
         if (!cpsRepeat.haveAwait) 
            CpsChunkBuilder.sync(asyncMonad, patternCode)
         else
            new CpsChunkBuilder[F,T](asyncMonad) {
               
               // TODO: add name to whileFun ?
               val createExpr = '{
                 def _whilefun(): F[T] = {
                   if (${cond}) 
                     ${cpsRepeat.chunkBuilder.flatMapIgnore('{ _whilefun() }).toExpr}
                   else
                     ${pure('{()}).toExpr.asInstanceOf[Expr[F[T]]]}
                 }
                 _whilefun()
               }

               override def create() = 
                            fromFExpr(createExpr)
               override def append[A:quoted.Type](e:CpsChunk[F,A]) =
                     flatMapIgnore(e.toExpr)
            }
       else // (cpsCond.haveAwait) 
         if (!cpsRepeat.haveAwait) {
            new CpsChunkBuilder[F,T](asyncMonad) {
               val createExpr: Expr[F[T]] = '{
                 def _whilefun(): F[T] = {
                   ${cpsCond.chunkBuilder.flatMap[T]( '{ c =>
                       if (c) {
                         $repeat 
                         _whilefun()
                       } else {
                         ${pure('{()}).toExpr.asInstanceOf[Expr[F[T]]]}
                       }
                    }
                   ).toExpr}
                 }
                 _whilefun()
               }
        
               override def create() = fromFExpr(createExpr)

               override def append[A:quoted.Type](e:CpsChunk[F,A]) =
                     flatMapIgnore(e.toExpr)
            }
         } else {
            new CpsChunkBuilder[F,T](asyncMonad) {
               val createExpr = '{
                 def _whilefun(): F[T] = {
                   ${cpsCond.chunkBuilder.flatMap[T]( '{ c =>
                       if (c) {
                         ${cpsRepeat.chunkBuilder.flatMapIgnore(
                             '{ _whilefun() }
                          ).toExpr}
                       } else {
                         ${pure('{()} ).toExpr.asInstanceOf[Expr[F[T]]]}
                       }
                    }).toExpr
                   }
                 }
                 _whilefun()
               }

               override def create() = fromFExpr(createExpr) 

               override def append[A:quoted.Type](e:CpsChunk[F,A]) =
                     flatMapIgnore(e.toExpr)

            }
         }
     }
     CpsExprResult[F,T](patternCode, builder, patternType, isAsync)
     

