package cps

import scala.quoted._
import scala.util.Try

trait AsyncMonad[F[_]] {

   def pure[T](t:T):F[T]

   def finalAwait[T](t:F[T]): Try[T]

   def map[A,B](fa:F[A])(f: A=>B):F[B]

   def flatMap[A,B](fa:F[A])(f: A=>F[B]):F[B]

   def error[A](e: Throwable): F[A]

   def suspend[A](thunk: =>F[A]): F[A]

   def delay[A](thunk: =>A): F[A]

}
