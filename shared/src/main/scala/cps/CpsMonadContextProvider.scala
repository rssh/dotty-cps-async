package cps

trait CpsMonadContextProvider[F[_]]:

    type Context <: CpsMonadContext[F]

    def  contextualize[A](fa: Context => F[A]): F[A]

