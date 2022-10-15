// part of dotty-cps-async
// (C) Ruslan Shevchenko <ruslan@shevchenko.kiev.ua>, 2019, 2020, 2021
package cps.macros.forest

import scala.quoted._

import cps._
import cps.macros._
import cps.macros.misc._

object ThrowTransform:

  /**
   *'''
   * '{ throw $ex }
   *'''
   **/
  def run[F[_]:Type,T:Type, C<:CpsMonadContext[F]:Type, S<:Throwable:Type](cpsCtx: TransformationContext[F,T,C],
                               ex: Expr[S]
                               )(using Quotes): CpsExpr[F,T] =
     import quotes.reflect._
     import util._
     import cpsCtx._
     val cpsEx = Async.nestTransform(ex, cpsCtx)

     val monadGen = cpsCtx.monadGen
     if (monadGen.supportsTryCatch) then
      if (!cpsEx.isAsync)  then
        // TODO: think, mb leave as is...
        // TDOD: pass origin to monadGen for right error message (?)
        CpsExpr.async[F,T](monadGen, monadGen.error(ex) )
      else
        CpsExpr.async[F,T](monadGen,
            cpsEx.flatMap[T]( '{ (ex:S) => ${monadGen.error('ex)} } ).transformed )
     else
       throw MacroError("this monad not support try/catch",ex)

