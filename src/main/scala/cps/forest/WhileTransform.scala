package cps.forest

import scala.quoted._

import cps._
 
object WhileTransform:

  /**
   *'''
   * '{ _root_.cps.await[F,$ftType]($ft) } 
   *'''
   **/
  def run[F[_]:Type,T:Type](cpsCtx: TransformationContext[F,T], 
                               cond: Expr[Boolean], repeat: Expr[Unit]
                               )(using qctx: QuoteContext): CpsExpr[F,T] =
     import qctx.tasty.{_, given _}
     import util._
     import cpsCtx._
     val cpsCond = Async.nestTransform(cond, cpsCtx, "C")
     val cpsRepeat = Async.nestTransform(repeat, cpsCtx, "W")
     val isAsync = cpsCond.isAsync || cpsRepeat.isAsync

     def uninline[X](x:Expr[X]):Expr[X] = 
              TransformUtil.skipInlined(x.unseal).seal.asInstanceOf[Expr[X]]

     val unitBuilder = {
       if (!cpsCond.isAsync)
         if (!cpsRepeat.isAsync) 
            CpsExpr.sync(monad, patternCode)
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
     

