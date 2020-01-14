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

     val unitBuilder = {
       if (!cpsCond.haveAwait)
         if (!cpsRepeat.haveAwait) 
            CpsChunkBuilder.sync(asyncMonad, patternCode)
         else
            CpsChunkBuilder.async[F,Unit](asyncMonad,
               // TODO: add name to whileFun ?
               '{
                 def _whilefun(): F[Unit] = {
                   if (${cond}) 
                     ${cpsRepeat.chunkBuilder.flatMapIgnore('{ _whilefun() }).toExpr}
                   else
                     ${asyncMonad}.pure(())
                 }
                 _whilefun()
               })
       else // (cpsCond.haveAwait) 
         if (!cpsRepeat.haveAwait) {
            CpsChunkBuilder.async[F,Unit](asyncMonad,
               '{
                 def _whilefun(): F[Unit] = {
                   ${cpsCond.chunkBuilder.flatMap[Unit]( '{ c =>
                       if (c) {
                         $repeat 
                         _whilefun()
                       } else {
                         ${asyncMonad}.pure(())
                       }
                    }
                   ).toExpr}
                 }
                 _whilefun()
               })
         } else {
            CpsChunkBuilder.async[F,Unit](asyncMonad,
               '{
                 def _whilefun(): F[Unit] = {
                   ${cpsCond.chunkBuilder.flatMap[Unit]( '{ c =>
                       if (c) {
                         ${cpsRepeat.chunkBuilder.flatMapIgnore(
                             '{ _whilefun() }
                          ).toExpr}
                       } else {
                         ${asyncMonad}.pure(())
                       }
                    }).toExpr
                   }
                 }
                 _whilefun()
               })
         }
     }
     val builder = unitBuilder.asInstanceOf[CpsChunkBuilder[F,T]]
     CpsExprResult[F,T](patternCode, builder, patternType, isAsync)
     

