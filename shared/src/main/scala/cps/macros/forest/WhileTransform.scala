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
       if (!cpsCond.isAsync) then
         if (!cpsRepeat.isAsync) then
            if (!cpsCond.isChanged && !cpsRepeat.isChanged)
               CpsExpr.sync(monadGen, patternCode, false)
            else 
               val term = While(cpsCond.syncOrigin.get.asTerm, cpsRepeat.syncOrigin.get.asTerm)
               CpsExpr.sync(monadGen, term.asExprOf[T], true)
         else
             val monad = monadGen.monadInstance.asExprOf[CpsMonad[F]]
             CpsExpr.async[F,Unit](monadGen, '{
              cps.runtime.WhileHelper.w01(${monad},${cond},${cpsRepeat.transformed})
             })  
       else // (cpsCond.isAsync) 
         val monad = monadGen.monadInstance.asExprOf[CpsMonad[F]]
         if (!cpsRepeat.isAsync) {
            CpsExpr.async[F,Unit](monadGen, '{
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
            CpsExpr.async[F,Unit](monadGen, 
            '{
              cps.runtime.WhileHelper.w11(${monad},${cpsCond.transformed},${cpsRepeat.transformed})
            })
         }
     }
     unitBuilder.asInstanceOf[CpsExpr[F,T]]
     

