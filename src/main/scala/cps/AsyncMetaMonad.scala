package cps

import scala.quoted._

trait AsyncMetaMonad[F[_]] {

   def pure[T:Type](t:Expr[T]):(given ctx:QuoteContext) => Expr[F[T]]

   def finalAwait[T:Type](t:Expr[F[T]]):(given ctx:QuoteContext) => Expr[T]

   def map[A,B](fa:F[A])(f: A=>B):F[B]

   def flatMap[A,B](fa:F[A])(f: A=>F[B]):F[B]

   def suspend[A](thunk: =>F[A]): F[A]

   def delay[A](thunk: =>A): F[A]
 

}
