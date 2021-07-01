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


@compileTimeOnly("await should be inside async block")
def await[F[_],T](f: F[T])(using am:CpsAwaitable[F]): T = ???


inline def async[F[_]](using inline am: CpsMonad[F]): macros.Async.InferAsyncArg[F] =
   new macros.Async.InferAsyncArg[F]

