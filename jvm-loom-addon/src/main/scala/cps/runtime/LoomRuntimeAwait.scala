package cps.runtime

import cps.*

trait LoomRuntimeAwait[F[_]] extends CpsRuntimeAwait[F] {
  
    def runAsync[A, C <: CpsTryMonadContext[F]](f: C => A)(m: CpsAsyncEffectMonad[F], ctx: C): F[A]

    def await[A](fa: F[A])(ctx: CpsTryMonadContext[F]): A

}
