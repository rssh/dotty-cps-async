package cps

import scala.quoted._
import scala.util.Try
import scala.concurrent.duration._

/**
 * Basic CpsMonad operations.
 * Implementing this typeclass is enough to use async/await with supports of
 * basic control-flow constructions (if, loops, but no exceptions).
 **/
trait CpsMonad[F[_]] {

   type WF[X] = F[X]

   def pure[T](t:T):F[T]

   def map[A,B](fa:F[A])(f: A=>B):F[B]

   def flatMap[A,B](fa:F[A])(f: A=>F[B]):F[B]

}


/**
 * If you monad supports this typeclass, than
 * you can use try/catch/finally inside await.
 **/
trait CpsTryMonad[F[_]] extends CpsMonad[F] {

   def error[A](e: Throwable): F[A]

   def restore[A](fa: F[A])(fx:Throwable => F[A]): F[A]

   def withAction[A](fa:F[A])(action: =>Unit):F[A] =
    flatMap(
       restore(fa){ ex => 
         try {
           action
           error(ex) 
         }catch{
           case ex1: Throwable => ex.addSuppressed(ex1)
             error(ex)
         }
       }
      ){x => 
        try{
          action
          pure(x)
        }catch{
          case ex: Throwable => error(ex)
        }
      }

}


/**
 * Monad, interpolable with Future.
 **/
trait CpsAsyncMonad[F[_]] extends CpsTryMonad[F] {

   /**
    * called by the source, which accept callback.
    * source is called immediatly in adoptCallbackStyle
    **/
   def adoptCallbackStyle[A](source: (Try[A]=>Unit) => Unit): F[A]

}


trait CpsSchedulingMonad[F[_]] extends CpsAsyncMonad[F] {


   /**
    * schedule execution of op somewhere.
    * Note, that characteristics of scheduler can vary.
    **/
   def spawn[A](op: =>F[A]): F[A]


}

trait CpsFulfillingMonad[F[_]] extends CpsAsyncMonad[F] {

   /**
    * block until monad will be finished or timeout will be expired.
    * Note, that using this operation inside async is dangerous.
    **/
   def fulfill[T](t:F[T], timeout: Duration): Option[Try[T]]

}


object CpsMonad:

  object ForSyntax:

    extension [F[_],T,S](x:F[T])(using m:CpsMonad[F]):

      def flatMap(f: T=>F[S]): F[S] =
         m.flatMap(x)(f)

      def map(f: T=>S): F[S] =
         m.map(x)(f)



