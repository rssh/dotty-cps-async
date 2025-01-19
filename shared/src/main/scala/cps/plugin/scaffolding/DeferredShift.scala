package cps.plugin.scaffolding

import cps.{CpsMonad, CpsTryMonadContext}

import scala.annotation.compileTimeOnly

@compileTimeOnly("this call should be eliminated by dotty-cps-async compiler plugin.  Check - if it enabled")
def deferredAsync[A, F[_], C <: CpsTryMonadContext[F]](x: A)(mc: C): F[A] = ???
