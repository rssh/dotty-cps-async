package cps.monads

import cps.*
import scala.annotation.implicitAmbiguous
import scala.util.Try


/**
 * FreeCpsMonad 
 * Typical pattern - use FreeCpsMonad and custom interpreter in test.
 **/
sealed trait FreeMonad[+T]

object FreeMonad {
  case class Pure[A](a:A) extends FreeMonad[A]
  case class FlatMap[A,B](fa: FreeMonad[A], f: A=>FreeMonad[B]) extends FreeMonad[B]
  case class Error(e: Throwable) extends FreeMonad[Nothing]
  case class FlatMapTry[A,B](fa: FreeMonad[A], f: Try[A]=>FreeMonad[B]) extends FreeMonad[B]

}


/**
 * Implementation for FreeCpsMonad.
 * It is intentionally defiend at top-level, to fire 'implicitAmbigious' error when
 * async is called without parameters and without select instance of CpsMonad in scope.
 **/
@implicitAmbiguous(
  """
 You use async without type parameter (i.e. async{ ... } instead async[F])
 Therefore exists multiple possible variants, definied in cps.monads.*
 If you really want use async wthout type parameter, define given instance of selected CpsMonad in the neareast scope
"""
)
given FreeCpsMonad: CpsTryMonad[FreeMonad] with CpsTryMonadInstanceContext[FreeMonad] with {

  type F[A] = FreeMonad[A]

  def pure[A](a:A): F[A] = FreeMonad.Pure(a)

  def map[A,B](fa:F[A])(f: A=>B): F[B] = FreeMonad.FlatMap(fa, (x:A) => pure(f(x)))

  def flatMap[A,B](fa: F[A])(f: A => F[B]): F[B] = FreeMonad.FlatMap(fa,f)

  def error[A](e: Throwable): F[A] = FreeMonad.Error(e)

  def flatMapTry[A,B](fa:F[A])(f: Try[A]=>F[B]): F[B] = FreeMonad.FlatMapTry(fa,f)

}

/**
 * FreeCpsMonad with async support.
 * Used when you want use test free monad with API which uses callbacks.
 * @tparam T
 */
sealed trait AsyncFreeMonad[+T] extends FreeMonad[T]


object AsyncFreeMonad {


  case class Pure[A](a: A) extends AsyncFreeMonad[A]
  case class FlatMap[A, B](fa: AsyncFreeMonad[A], f: A => AsyncFreeMonad[B]) extends AsyncFreeMonad[B]
  case class Error(e: Throwable) extends AsyncFreeMonad[Nothing]
  case class FlatMapTry[A, B](fa: AsyncFreeMonad[A], f: Try[A] => AsyncFreeMonad[B]) extends AsyncFreeMonad[B]
  case class AdoptCallbackStyle[A](source:(Try[A]=>Unit)=>Unit) extends AsyncFreeMonad[A]

  given CpsAsyncMonad[AsyncFreeMonad] with CpsTryMonadInstanceContext[AsyncFreeMonad] with
    def pure[A](a:A): AsyncFreeMonad[A] = AsyncFreeMonad.Pure(a)
    def map[A,B](fa:AsyncFreeMonad[A])(f: A=>B): AsyncFreeMonad[B] =
        AsyncFreeMonad.FlatMap(fa, (x:A) => pure(f(x)))
    def flatMap[A,B](fa: AsyncFreeMonad[A])(f: A => AsyncFreeMonad[B]): AsyncFreeMonad[B] =
        AsyncFreeMonad.FlatMap(fa,f)
    def error[A](e: Throwable): AsyncFreeMonad[A] = AsyncFreeMonad.Error(e)
    def flatMapTry[A,B](fa:AsyncFreeMonad[A])(f: Try[A]=>AsyncFreeMonad[B]): AsyncFreeMonad[B] =
        AsyncFreeMonad.FlatMapTry(fa,f)
    def adoptCallbackStyle[A](source: (Try[A]=>Unit) => Unit): AsyncFreeMonad[A] =
        AsyncFreeMonad.AdoptCallbackStyle(source)

}


