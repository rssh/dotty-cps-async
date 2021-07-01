package cps.macros.forest

import scala.quoted._

import cps._
import cps.macros._
 
object WhileTransform:

  /**
   *'''
   * '{ _root_.cps.await[F,$ftType]($ft) } 
   *'''
   **/
  def run[F[_]:Type,T:Type](cpsCtx: TransformationContext[F,T], 
                               cond: Expr[Boolean], repeat: Expr[Unit]
                               )(using Quotes): CpsExpr[F,T] =
     import quotes.reflect._
     import util._
     import cpsCtx._
     val cpsCond = Async.nestTransform(cond, cpsCtx)
     val cpsRepeat = Async.nestTransform(repeat, cpsCtx)
     val isAsync = cpsCond.isAsync || cpsRepeat.isAsync

     val unitBuilder = {
       if (!cpsCond.isAsync)
         if (!cpsRepeat.isAsync) 
            if (!cpsCond.isChanged && !cpsRepeat.isChanged)
               CpsExpr.sync(monad, patternCode, false)
            else 
               val term = While(cpsCond.syncOrigin.get.asTerm, cpsRepeat.syncOrigin.get.asTerm)
               CpsExpr.sync(monad, term.asExprOf[T], true)
         else
            CpsExpr.async[F,Unit](monad,
               // TODO: add name to whileFun ?
               '{
                 def _whilefun(): F[Unit] = {
                   if (${cond}) 
                     ${cpsRepeat.flatMapIgnore(
                          '{ _whilefun() }
                      ).transformed}
                   else
                     ${monad}.pure(())
                 }
                 _whilefun()
               })
       else // (cpsCond.isAsync) 
         if (!cpsRepeat.isAsync) {
            CpsExpr.async[F,Unit](monad,
               '{
                 def _whilefun(): F[Unit] = {
                   ${cpsCond.flatMap[Unit]( '{ c =>
                       if (c) {
                         $repeat 
                         _whilefun()
                       } else {
                         ${monad}.pure(())
                       }
                    }
                   ).transformed}
                 }
                 _whilefun()
               })
         } else {
            CpsExpr.async[F,Unit](monad,
               '{
                 def _whilefun(): F[Unit] = {
                   ${cpsCond.flatMap[Unit]('{ (c: Boolean) =>
                       if (c) {
                         ${cpsRepeat.flatMapIgnore(
                             '{ _whilefun() }
                          ).transformed}
                       } else {
                         ${monad}.pure(())
                       }
                    }).transformed 
                   }
                 }
                 _whilefun()
               })
         }
     }
     unitBuilder.asInstanceOf[CpsExpr[F,T]]
     

