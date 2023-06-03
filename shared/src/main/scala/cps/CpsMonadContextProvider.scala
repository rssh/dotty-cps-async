package cps

trait CpsMonadContextProvider[F[_]]:

    type Context <: CpsTryMonadContext[F]

    def  contextualize[A](m: CpsMonad[F], fa: Context => F[A]): F[A]

