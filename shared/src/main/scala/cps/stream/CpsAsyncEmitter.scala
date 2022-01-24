package cps.stream

import cps.{*,given}

/**
 * Emitter which should be a parameter of asyncStream expression.
 *
 * ```
 *   asyncStream[AsyncList[F,Int]] { out =>
 *      for(i <- 1 to 10) 
 *         out.emit(i)
 *   }
 * ```
 * Here out have a `CpsAsyncEmitter[AsyncList[F,Int],F,E]` type.
 *@see [cps.   asyncStream]
 **/
trait CpsAsyncEmitter[F[_]: CpsAsyncMonad, E]:

   transparent inline def emit(v:E)(using CpsMonadContext[F]): Unit =
      await(emitAsync(v))

   def emitAsync(v:E): F[Unit]


