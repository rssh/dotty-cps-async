package cps


import scala.annotation.*


// use cpsContext
//@experimental
//@capability
//class CpsTransform[F[_]](val m:CpsTryMonad[F])


@experimental
@compileTimeOnly("await should be used inside async block")
def cpsAwait[F[_],A,G[_]](fa:F[A])(using CpsMonadContext[G],CpsMonadConversion[F,G]): A =
  ???


@experimental
inline def cpsAsync[F[_]](using m:CpsTryMonad[F]) =
  new CpsTransform.InfernAsyncArg


@experimental  
object CpsTransform {

  class InfernAsyncArg[F[_],C<:CpsMonadContext[F]](using val am: CpsMonad.Aux[F,C]) {
    
      @compileTimeOnly("this call should be eliminated by cps plugin")
      def apply[A](expr: C ?=> A): F[A] =
        ???

      def applyM1[A](expr: C ?=> F[A]): F[A] =
        am.apply(c => expr(using c))

  }

  //given mk[F[_]](using m:CpsTryMonad[F]):CpsTransform[F] = new CpsTransform[F](m)

}




