package cps.plugin

import cps.*

import scala.annotation.compileTimeOnly


/**
 * Placehodler, which delegate async transformation to compiler plugin
 */
@compileTimeOnly("this call shpuldbe eliminated by cpsPlugin")
def cpsAsyncApply[F[_],T,C <: CpsMonadContext[F]](am: CpsMonad.Aux[F,C], f: C ?=> T): F[T] =
  ???