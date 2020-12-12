package cps.monads

import cps._
import scala.concurrent._
import scala.concurrent.duration._
import scala.quoted._
import scala.util._

given FutureAsyncMonad(using ExecutionContext): CpsSchedulingMonad[Future] with

   type F[+T] = Future[T]

   override type WF[T] = F[T]

   def pure[T](t:T):Future[T] = Future(t)

   def map[A,B](fa:F[A])(f: A=>B):F[B] =
        fa.map(f)

   def flatMap[A,B](fa:F[A])(f: A=>F[B]):F[B] =
        fa.flatMap(f)

   def error[A](e: Throwable): F[A] =
        Future.failed(e)
   
   override def mapTry[A,B](fa:F[A])(f: Try[A]=>B): F[B] =
        fa.transform{ v => Success(f(v)) }

   def flatMapTry[A,B](fa:F[A])(f: Try[A]=>F[B]): F[B] =
        fa.transformWith{ v => f(v) }


   override def restore[A](fa: F[A])(fx:Throwable => F[A]): F[A] =
        fa.recoverWith{ case ex => fx(ex) }

   def adoptCallbackStyle[A](source: (Try[A]=>Unit) => Unit): F[A] =
        val p = Promise[A]
        source(p.complete(_))
        p.future

   def spawn[A](op: => F[A]): F[A] =
        val p = Promise[A]
        summon[ExecutionContext].execute( () => p.completeWith(op) )
        p.future


   def fulfill[T](t:F[T], timeout: Duration): Option[Try[T]] =
        try
          Await.ready(t, timeout)
          t.value
        catch
          case ex: TimeoutException => t.value

   def executionContext = summon[ExecutionContext]

object FutureAsyncMonad:

   given ImplicitAwait: cps.features.implicitAwait.IsPossible[Future] with {}


given fromFutureConversion[G[_]](using ExecutionContext, CpsAsyncMonad[G]): CpsMonadConversion[Future,G] =
   new CpsMonadConversion[Future, G] {
     override def apply[T](mf: CpsMonad[Future], mg: CpsMonad[G], ft:Future[T]): G[T] =
           summon[CpsAsyncMonad[G]].adoptCallbackStyle(
                                         listener => ft.onComplete(listener) )
   }


given toFutureConversion[F[_]](using ExecutionContext, CpsSchedulingMonad[F]): CpsMonadConversion[F,Future] =
   new CpsMonadConversion[F, Future] {
     override def apply[T](mf: CpsMonad[F], mg: CpsMonad[Future], ft:F[T]): Future[T] =
        val p = Promise[T]()
        val u = summon[CpsSchedulingMonad[F]].restore(
                        mf.map(ft)( x => p.success(x) )
                 )(ex => mf.pure(p.failure(ex)) )
        // we need from uMonad some method to schedule ?
        //   TODO: rething monad interfaces, maybe we shoud have something like: "adopt" instead spawn.
        //     look's like for for cats IO such function can't exists, but application can provide runtime
        //     which will called all spawns for running
        summon[CpsSchedulingMonad[F]].spawn(u)
        p.future
   }




