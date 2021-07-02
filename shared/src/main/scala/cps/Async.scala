/*
 * dotty-cps-async: https://github.com/rssh/dotty-cps-async
 *
 * (C) Ruslan Shevchenko <ruslan@shevchenko.kiev.ua>, Kyiv, 2020, 2021
 */
package cps

import scala.language.implicitConversions

import scala.annotation._
import scala.quoted._
import scala.compiletime._

/*
 * Pseudofunction, which can be used inside async block, to 'await' (i.e. receive value of `t:T` from `ft:F[T]`).
 **/
@compileTimeOnly("await should be inside async block")
def await[F[_],T](f: F[T])(using am:CpsAwaitable[F]): T = ???

/*
 * async block, which can contains awaits
 **/
inline def async[F[_]](using inline am: CpsMonad[F]): macros.Async.InferAsyncArg[F] =
   new macros.Async.InferAsyncArg[F]

