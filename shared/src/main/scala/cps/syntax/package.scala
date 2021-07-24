/*
 * dotty-cps-async: https://github.com/rssh/dotty-cps-async
 *
 * (C) Ruslan Shevchenko <ruslan@shevchenko.kiev.ua>, Kyiv, 2020, 2021
 */
package cps.syntax

import cps.*

/**
 * short synonym of `await`
 **/
extension [F[_],T](ft:F[T])(using CpsAwaitable[F])

    transparent inline def unary_! :T = await[F,T](ft)

