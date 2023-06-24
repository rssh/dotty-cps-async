/*
 * dotty-cps-async: https://github.com/rssh/dotty-cps-async
 *
 * (C) Ruslan Shevchenko <ruslan@shevchenko.kiev.ua>, Kyiv, 2020, 2021, 2022, 2023
 */
package cps

import scala.annotation._
import scala.quoted._
import scala.compiletime._

import cps.plugin.annotation.CpsNotChange


/**
 * Pseudofunction, which can be used inside async block, to 'await' (i.e. receive value of `t:T` from `ft:F[T]`).
 **/
@compileTimeOnly("await should be inside async block")
def await[F[_],T,G[_]](f: F[T])(using ctx: CpsMonadContext[G], conversion: CpsMonadConversion[F,G]): T = ???

/**
 * Pseudofunction, which can be used inside async block or in function with CpsDirect[F] context parameter, to 'asynchronize computation' (i.e. receive value of `F[T]` from `t:FT`).
 * The main usage is in direct mode, where all computations are 'awaited' by default. Inside async block can be viewed as empty wrapper.
 * @param t - expression in direct mode
 * @param ctx - Monad context
 * @tparam F - monad
 * @tparam T - type of expression
 * @return - <code>t</code> expression represented in monadic form
 * @todo - currently attempt to makr t context-dependend(i.e. pss to asynchronized t: CpsMonadContext[F] ?=>T) leads to error during typing.
 *       Need to investigate and submit bug to dotty.
 */
@compileTimeOnly("asynchronized should be inside async block")
@experimental
@CpsNotChange
def asynchronized[F[_],T](t: CpsDirect[F] ?=> T)(using ctx: CpsDirect[F]): F[T] = ???


/**
 * async block, which can contains awaits.
 * better look on this as the first part of the next signature:
 * ```
 *    async[F](using CpsMonad[F])[T](inline body:T):F[T]
 * ```
 * i.e. async return a transitional object, which accepts body and perform async transform with the given 
 * `CpsMonad[F]`.
 **/
transparent inline def async[F[_]](using am: CpsMonad[F]) =
   macros.Async.InferAsyncArg(using am)


/**
 * Synonym for `async` which can be better operation name for non-computation monads.
 **/
transparent inline def reify[F[_]](using am: CpsMonad[F]) =
   macros.Async.InferAsyncArg(using am)

/**
 * Synonym for `await` 
 **/
transparent inline def reflect[F[_],T,G[_]](f: F[T])(using inline ctx: CpsMonadContext[G], inline conv: CpsMonadConversion[F,G]): T =
   await[F,T,G](f)


@experimental
@CpsNotChange
transparent inline def reifed[F[_],T](t: CpsDirect[F] ?=> T)(using inline ctx: CpsDirect[F]): F[T] =
   asynchronized[F,T](t)
