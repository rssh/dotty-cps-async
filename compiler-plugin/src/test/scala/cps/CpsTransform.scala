package cps

import scala.annotation.*

@capability
class CpsTransform[F[_]](m:CpsTryMonad[F]) {

  @compileTimeOnly("this call should be eliminated by cps plugin")
  def await[A](fa:F[A]): {this} A =
    ???

}


transparent inline def cpsAsync[F[_]](using m:CpsTryMonad[F]) =
  CpsTransform.InfernAsyncArg

object CpsTransform {

  class InfernAsyncArg[F[_],C<:CpsMonadContext[F]](using val am: CpsMonad.Aux[F,C]) {
    
      @compileTimeOnly("this call should be eliminated by cps plugin")
      def apply[A](expr: (CpsTransform[F], C) ?=> A): F[A]

  }

  given mk[F[_]](using m:CpsTryMonad[F]):CpsTransform[F] = new CpsTransform[F](m)


}
