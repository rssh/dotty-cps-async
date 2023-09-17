package cps

trait CpsMonadContextInclusion[F[_],G[_]] {

  def apply[T](fctx: CpsTryMonadContext[F])(fun: CpsTryMonadContext[G] => G[T]):F[T]

}

object CpsMonadContextInclusion {

  given byConversion[F[_],G[_]](using gfc: CpsMonadConversion[G,F], gm: CpsTryMonad[G]): CpsMonadContextInclusion[F,G] =
    new CpsMonadContextInclusion[F,G] {
      override def apply[T](fctx: CpsTryMonadContext[F])(fun: CpsTryMonadContext[G] => G[T]):F[T] =
        gfc.apply(gm.apply(fun))
    }

  given byIdentity[F[_]]: CpsMonadContextInclusion[F,F] =
    new CpsMonadContextInclusion[F,F] {
      override def apply[T](fctx: CpsTryMonadContext[F])(fun: CpsTryMonadContext[F] => F[T]): F[T] =
        fun(fctx)
    }

}
