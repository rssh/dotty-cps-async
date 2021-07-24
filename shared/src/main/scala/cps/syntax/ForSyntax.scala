/*
 * dotty-cps-async: https://github.com/rssh/dotty-cps-async
 *
 * (C) Ruslan Shevchenko <ruslan@shevchenko.kiev.ua>, Kyiv, 2020, 2021
 */
package cps.syntax

import cps._

/**
 * allows to use for syntax on generic monad-wrapped values.
 **/
extension [F[_],T,S](x:F[T])(using m:CpsMonad[F])

   def flatMap(f: T=>F[S]): F[S] =
      m.flatMap(x)(f)

   def map(f: T=>S): F[S] =
      m.map(x)(f)



