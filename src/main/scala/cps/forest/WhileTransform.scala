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
                               )(given qctx: QuoteContext): CpsExpr[F,T] =
     import qctx.tasty.{_, given}
     import util._
     import cpsCtx._
     val cpsCond = Async.rootTransform(cond, asyncMonad, false)
     val cpsRepeat = Async.rootTransform(repeat, asyncMonad, false)
     val isAsync = cpsCond.isAsync || cpsRepeat.isAsync

     val unitBuilder = {
       if (!cpsCond.isAsync)
         if (!cpsRepeat.isAsync) 
            CpsExpr.sync(asyncMonad, patternCode)
         else
            CpsExpr.async[F,Unit](asyncMonad,
               // TODO: add name to whileFun ?
               '{
                 def _whilefun(): F[Unit] = {
                   if (${cond}) 
                     ${cpsRepeat.flatMapIgnore('{ _whilefun() }).transformed}
                   else
                     ${asyncMonad}.pure(())
                 }
                 _whilefun()
               })
       else // (cpsCond.isAsync) 
         if (!cpsRepeat.isAsync) {
            CpsExpr.async[F,Unit](asyncMonad,
               '{
                 def _whilefun(): F[Unit] = {
                   ${cpsCond.flatMap[Unit]( '{ c =>
                       if (c) {
                         $repeat 
                         _whilefun()
                       } else {
                         ${asyncMonad}.pure(())
                       }
                    }
                   ).transformed}
                 }
                 _whilefun()
               })
         } else {
            CpsExpr.async[F,Unit](asyncMonad,
               '{
                 def _whilefun(): F[Unit] = {
                   ${cpsCond.flatMap[Unit]( '{ c =>
                       if (c) {
                         ${cpsRepeat.flatMapIgnore(
                             '{ _whilefun() }
                          ).transformed}
                       } else {
                         ${asyncMonad}.pure(())
                       }
                    }).transformed
                   }
                 }
                 _whilefun()
               })
         }
     }
     unitBuilder.asInstanceOf[CpsExpr[F,T]]
     

