/**
 * Monadless syntax support: see  https://github.com/monadless/monadless
 **/
package cps.syntax.monadless

import scala.annotation._

import cps.*

/**
 * Synonym for `async` 
 **/
transparent inline def lift[F[_]](using am: CpsMonad[F]) =
  macros.Async.InferAsyncArg(using am)



extension [F[_], T, G[_]](f: F[T])(using ctx: CpsMonadContext[G], conversion: CpsMonadConversion[F, G])
  /**
   * Synonym for `await`
   * */
  transparent inline def unlift: T = await(f)
