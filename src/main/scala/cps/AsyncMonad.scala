package cps

import scala.quoted._
import scala.util.Try
import scala.concurrent.duration._


trait AsyncMonad[F[_]] {

   def pure[T](t:T):F[T]

   def map[A,B](fa:F[A])(f: A=>B):F[B]

   def flatMap[A,B](fa:F[A])(f: A=>F[B]):F[B]

   // TODO: not all interesting monads can handle error
   def error[A](e: Throwable): F[A]

   def restore[A](fa: F[A])(fx:Throwable => F[A]): F[A]

   def withAction[A](fa:F[A])(action: =>Unit):F[A]



}
