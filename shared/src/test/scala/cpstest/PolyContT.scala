package cpstest

import cps.CpsTryMonad

sealed trait PolyContT[F[_], A] {

  basePolyContT =>

  def run[B](f: A => F[B]): F[B]


  def map[B](f: A => B)(using m:CpsTryMonad[F]): PolyContT[F,B] =
    new PolyContT[F,B]{
      override def run[C](g: B => F[C]): F[C] =
        m.flatMap(basePolyContT.run(x => summon[CpsTryMonad[F]].pure(f(x))))(g)
    }



}



object PolyContT {

    sealed trait ContTStep[F[_],A,B] extends Function1[A, F[B]]  {

    }

    case class FunStep[F[_],A,B](f: A => F[B]) extends ContTStep[F,A,B] {
      def apply(a: A): F[B] = f(a)
    }

    case class FlatMapStep[F[_]:CpsTryMonad,A,B,C](f: ContTStep[F,A,B], g: ContTStep[F,B,C]) extends ContTStep[F,A,C] {
      def apply(a: A): F[C] =
        summon[CpsTryMonad[F]].flatMap(f(a))(g)
    }


}