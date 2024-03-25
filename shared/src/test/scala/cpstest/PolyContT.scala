package cpstest

import scala.util.*
import cps.*
import cpstest.PolyContT.PolyStepTry


sealed trait PolyContT[F[_], A] {

  basePolyContT =>

  def run[B](f: A => F[B])(using m: CpsTryMonad[F]): F[B] =
    runTry{
      case Success(a) => f(a)
      case Failure(ex) => m.fromTry(Failure(ex))
    }

  def runTry[B](f: Try[A]=>F[B]): F[B]

  def map[B](f: A => B)(using m:CpsTryMonad[F]): PolyContT[F,B] =
    new PolyContT[F,B]{
      override def runTry[C](g: Try[B] => F[C]): F[C] =
        m.flatMapTry(basePolyContT.run(x => summon[CpsTryMonad[F]].pure(f(x))))(g)
    }

  def flatMap[B](f: A => PolyContT[F,B])(using m:CpsTryMonad[F]): PolyContT[F,B] =
    new PolyContT[F,B]{
      override def runTry[C](g: Try[B]=> F[C]): F[C] =
        basePolyContT.run(a => f(a).runTry(g))
    }

  def flatMapTry[B](f: Try[A] => PolyContT[F,B])(using m:CpsTryMonad[F]): PolyContT[F,B] =
    new PolyContT[F,B]{
      override def runTry[C](g: Try[B]=> F[C]): F[C] =
        basePolyContT.runTry(a => f(a).runTry(g))
    }


  def shiftTry(poly: PolyStepTry[F,A])(using m:CpsTryMonad[F]): PolyContT[F,A] =
    new PolyContT[F,A]{
      override def runTry[B](f: Try[A] => F[B]): F[B] =
        poly.run(f)
    }

}



object PolyContT {

  
    trait ContTStep[F[_],A,B] extends Function1[A, F[B]]

    case class FunStep[F[_],A,B](f: A => F[B]) extends ContTStep[F,A,B] {
      def apply(a: A): F[B] = f(a)
    }

    case class FlatMapStep[F[_]:CpsTryMonad,A,B,C](f: ContTStep[F,A,B], g: ContTStep[F,B,C]) extends ContTStep[F,A,C] {
      def apply(a: A): F[C] =
        summon[CpsTryMonad[F]].flatMap(f(a))(g)
    }

    trait PolyStepTry[F[_],A] {
        def run[B](k: (Try[A]=>F[B])): F[B]
    }

  
}