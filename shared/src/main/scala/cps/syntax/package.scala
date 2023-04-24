/*
 * dotty-cps-async: https://github.com/rssh/dotty-cps-async
 *
 * (C) Ruslan Shevchenko <ruslan@shevchenko.kiev.ua>, Kyiv, 2020, 2021,2022
 */
package cps.syntax

import cps.*

/**
 * short synonym of `await`
 * It can be helpful when monad or environment does not support automatic coloring, but the default `await` 
 * syntax is too heavy. 
 **/
extension [F[_],T,G[_]](ft:F[T])(using CpsMonadContext[G], CpsMonadConversion[F,G])

    transparent inline def unary_! :T = await[F,T,G](ft)

