/*
 * dotty-cps-async: https://github.com/rssh/dotty-cps-async
 *
 * (C) Ruslan Shevchenko <ruslan@shevchenko.kiev.ua>, Kyiv, 2020, 2021
 */
package cps

import cps.stream.*


/**
 * Generator syntax.
 * usage:
 * ```
 * val s = asyncStream[fs.Stream[IO,Int]] { out =>
 *    for(i <- 1 to N) out.emit(i)
 * }
 * ```
 **/   
transparent inline def asyncStream[R](using a: CpsAsyncEmitAbsorber[R]) =
   AsyncStreamHelper(a)
