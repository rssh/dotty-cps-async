package cps.streamlike

import cps._

trait Emitter[F[_]:CpsAsyncMonad, T]:

   def emitAsync(v:T): F[Unit]

   transparent inline def emit(v:T): Unit =
      await(emitAsync(v))


