package cps

import scala.quoted._
import scala.util._
import scala.util.control.NonFatal
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

   def flatMapTry[A,B](fa:F[A])(f: Try[A] => F[B]):F[B]

   def mapTry[A,B](fa:F[A])(f: Try[A] => B):F[B] =
       flatMapTry(fa)(x => pure(f(x)))

   def restore[A](fa: F[A])(fx:Throwable => F[A]): F[A] =
         flatMapTry[A,A](fa){ x =>
           x match
             case Success(a) => pure(a)
             case Failure(e) => try{
                                  fx(e)
                                }catch{
                                  case NonFatal(ex) => error(ex)
                                }
         }


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


   def withAsyncAction[A](fa:F[A])(action: => F[Unit]):F[A] =
    flatMap(
       restore(fa){ ex =>
         flatMap(
           restore(tryImpure(action)){
             ex1 => ex.addSuppressed(ex1)
             error(ex)
           })(_ => error(ex))
       }
    ){ x =>
        map(tryImpure(action))(_ => x)
    }

   def tryPure[A](a: =>A):F[A] =
       try {
         pure(a)
       } catch {
         case ex: Throwable => error(ex)
       }

   def tryImpure[A](a: =>F[A]):F[A] =
       try {
         a
       } catch {
         case ex: Throwable => error(ex)
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

trait CpsProgressingMonad[F[_]] extends CpsAsyncMonad[F] {

   /**
    * progress and return control to user.
    **/
   def progress[T](t:F[T], timeout: Duration): Either[F[T],Try[T]]

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

    extension [F[_],T,S](x:F[T])(using m:CpsMonad[F])

      def flatMap(f: T=>F[S]): F[S] =
         m.flatMap(x)(f)

      def map(f: T=>S): F[S] =
         m.map(x)(f)



