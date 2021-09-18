package cps

trait CpsMonadContextProvider[F[_]]:

    type Context

    def  contextualize[A](fa: Context => F[A]): F[A]


