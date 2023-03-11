package cps

trait CpsMonadContextProvider[F[_]]:

    type Context <: CpsMonadContext[F]

    def  contextualize[A](m: CpsMonad[F], fa: Context => F[A]): F[A]

