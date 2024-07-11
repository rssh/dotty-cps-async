package cps.plugin

import cps.*
import cps.stream.{CpsAsyncEmitAbsorber, CpsAsyncEmitter}

import scala.annotation.compileTimeOnly


/**
 * Placehodler, which delegate async transformation to compiler plugin
 */
@compileTimeOnly("this call should be eliminated by cpsPlugin")
def cpsAsyncApply[F[_],T,C <: CpsMonadContext[F]](am: CpsMonad.Aux[F,C], f: C ?=> T): F[T] =
  ??? 

@compileTimeOnly("this call should be eliminated by cpsPlugin")
def cpsAsyncStreamApply[R, F[_], T, C <: CpsMonadContext[F]](absorber: CpsAsyncEmitAbsorber.Aux[R,F,C,T], f: C ?=> CpsAsyncEmitter[F,T] => Unit): R =
  ???

//def cpsAsyncApplyNC[F[_],T](am: CpsMonad[F], f: => T): F[T] =
//  ??? 
