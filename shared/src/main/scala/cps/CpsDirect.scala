package cps


class CpsDirect[F[_]](val context: CpsTryMonadContext[F]) extends  AnyVal {

  def monad: CpsMonad[F] = context.monad

  def throwMonad: CpsThrowMonad[F] = context.monad

  def tryMonad: CpsTryMonad[F] = context.monad

}


object CpsDirect {

  given direct[F[_]](using context: CpsTryMonadContext[F]): CpsDirect[F] = new CpsDirect[F](context)

}
