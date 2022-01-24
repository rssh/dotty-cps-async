package cps.macros.forest

import scala.quoted._

import cps._
import cps.macros._
 
object WhileTransform:

  /**
   * while
   **/
  def run[F[_]:Type,T:Type,C<:CpsMonadContext[F]:Type](cpsCtx: TransformationContext[F,T,C], 
                               cond: Expr[Boolean], repeat: Expr[Unit]
                               )(using Quotes): CpsExpr[F,T] =
     import quotes.reflect._
     import util._
     import cpsCtx._
     val cpsCond = Async.nestTransform(cond, cpsCtx)
     val cpsRepeat = Async.nestTransform(repeat, cpsCtx)
     val isAsync = cpsCond.isAsync || cpsRepeat.isAsync

     val DEBUG = true

     val unitBuilder = {
       if (!cpsCond.isAsync)
         if (!cpsRepeat.isAsync) 
            if (!cpsCond.isChanged && !cpsRepeat.isChanged)
               CpsExpr.sync(monad, patternCode, false)
            else 
               val term = While(cpsCond.syncOrigin.get.asTerm, cpsRepeat.syncOrigin.get.asTerm)
               CpsExpr.sync(monad, term.asExprOf[T], true)
         else
            /*
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
             */
             CpsExpr.async[F,Unit](monad, '{
              cps.runtime.WhileHelper.w01(${monad},${cond},${cpsRepeat.transformed})
             })  
       else // (cpsCond.isAsync) 
         if (!cpsRepeat.isAsync) {
            /*
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
            */
            CpsExpr.async[F,Unit](monad, '{
              cps.runtime.WhileHelper.w10(${monad},${cpsCond.transformed},${repeat})
            })
         } else {
            /*
            * TODO: submit big to doffy
            val retval = CpsExpr.async[F,Unit](monad,
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
            */   
            CpsExpr.async[F,Unit](monad, 
            '{
              cps.runtime.WhileHelper.w11(${monad},${cpsCond.transformed},${cpsRepeat.transformed})
            })
         }
     }
     unitBuilder.asInstanceOf[CpsExpr[F,T]]
     

