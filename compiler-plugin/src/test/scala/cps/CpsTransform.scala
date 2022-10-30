package cps

import scala.annotation.*

// workarround against https://github.com/lampepfl/dotty/issues/16129
@experimental
object E {

@experimental
@capability
class CpsTransform[F[_]](val m:CpsTryMonad[F])



@experimental
@compileTimeOnly("await should be used inside async block")
def cpsAwait[F[_],A,G[_]](fa:F[A])(using CpsTransform[G],CpsMonadConversion[F,G]): A =
  ???

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



