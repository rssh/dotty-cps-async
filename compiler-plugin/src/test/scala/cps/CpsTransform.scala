package cps

import scala.annotation.*

// workarround against https://github.com/lampepfl/dotty/issues/16129
@experimental
object E {

@experimental
@capability
class CpsTransform[F[_]](m:CpsTryMonad[F]) {

  @compileTimeOnly("this call should be eliminated by cps plugin")
  def await[A](fa:F[A]):  A =
    ???

}

@experimental
inline def cpsAwait[F[_],A](fa:F[A]): CpsTransform[F] ?=> A =
  summon[CpsTransform[F]].await(fa)

@experimental
inline def cpsAsync[F[_]](using m:CpsTryMonad[F]) =
  new CpsTransform.InfernAsyncArg

@experimental  
object CpsTransform {

  class InfernAsyncArg[F[_],C<:CpsMonadContext[F]](using val am: CpsMonad.Aux[F,C]) {
    
      @compileTimeOnly("this call should be eliminated by cps plugin")
      def apply[A](expr: (CpsTransform[F], C) ?=> A): F[A] = ???

  }

  given mk[F[_]](using m:CpsTryMonad[F]):CpsTransform[F] = new CpsTransform[F](m)

}

}//E



