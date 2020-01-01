package cps

import scala.concurrent._
import scala.quoted._

object FutureMetaMonad extends AsyncMetaMonad[Future] {

   type F = Future

   def pure[T:Type](t:Expr[T]):(given ctx:QuoteContext) => Expr[F[T]] = 
     '{ Future.successful(${t}) }

   def finalAwait[T:Type](t:Expr[Future[T]]):(given ctx:QuoteContext) => Expr[T] =
     import scala.concurrent.duration._
     '{ Await.result(${t}, Duration.Inf ) }

   def map[A,B](fa:F[A])(f: A=>B):F[B] = ???

   def flatMap[A,B](fa:F[A])(f: A=>F[B]):F[B] = ???

   def suspend[A](thunk: =>F[A]): F[A] = ???

   def delay[A](thunk: =>A): F[A] = ???


}
