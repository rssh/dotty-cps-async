/*
 * dotty-cps-async: https://github.com/rssh/dotty-cps-async
 *
 * (C) Ruslan Shevchenko <ruslan@shevchenko.kiev.ua>, Kyiv, 2020, 2021, 2022
 */
package cps

import scala.annotation._
import scala.quoted._
import scala.compiletime._


/**
 * Pseudofunction, which can be used inside async block, to 'await' (i.e. receive value of `t:T` from `ft:F[T]`).
 **/
@compileTimeOnly("await should be inside async block")
def await[F[_],T,G[_]](f: F[T])(using ctx: CpsMonadContext[G], conversion: CpsMonadConversion[F,G]): T = ???

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

//transparent inline def async[F[_]](using am: CpsTryMonad[F]) =
//   macros.Async.InferAsyncArg1(using am)


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

