package cps.monads

import cps._
import java.util.concurrent.CompletableFuture
import scala.util.Try
import scala.util.Failure
import scala.util.Success
import scala.util.control.NonFatal


given CompletableFutureCpsMonad: CpsSchedulingMonad[CompletableFuture] with CpsMonadInstanceContext[CompletableFuture] with {

   def pure[T](t:T):CompletableFuture[T] =
         CompletableFuture.completedFuture(t).nn

   def map[A,B](fa:CompletableFuture[A])(f: A=>B):CompletableFuture[B] =
        fa.thenApplyAsync(a => 
               f(a.nn)
        ).nn  // TODO:  dotty bug report: Function to java.util.Function

   def flatMap[A,B](fa:CompletableFuture[A])(f: A=> CompletableFuture[B]):CompletableFuture[B] =
        fa.thenComposeAsync(a => f(a.nn)).nn


   def error[A](e: Throwable): CompletableFuture[A] =
        CompletableFuture.failedFuture(e).nn

   override def mapTry[A,B](fa:CompletableFuture[A])(f: Try[A]=>B):CompletableFuture[B] =
        fa.handle{ (v,e) =>
            if (e == null) then
               f(Success(v.nn))
            else
               f(Failure(e.nn))
        }.nn.toCompletableFuture.nn

   override def flatMapTry[A,B](fa:CompletableFuture[A])(f: Try[A]=>CompletableFuture[B]):CompletableFuture[B] =
        val retval = new CompletableFuture[B]
        fa.handle{ (v,e) =>
          if (e == null) then
            try
              f(Success(v.nn)).handle{ (v1, e1) =>
                 if (e1 == null) then
                    retval.complete(v1.nn)
                 else
                    retval.completeExceptionally(e1.nn)
              } 
            catch
              case NonFatal(ex) =>
                 retval.completeExceptionally(ex)
          else
            try
              f(Failure(e.nn)).handle{ (v1,e1) =>
                 if (e1 == null) then
                    retval.complete(v1.nn)
                 else
                    retval.completeExceptionally(e1.nn)
              }
            catch
              case NonFatal(ex) =>
                 retval.completeExceptionally(ex)
        }
        retval


   override def restore[A](fa: CompletableFuture[A])(fx:Throwable => CompletableFuture[A]): CompletableFuture[A] =
        val retval = new CompletableFuture[A]
        fa.handle{ (v,e) =>
          if (e == null) then
             retval.complete(v.nn)
          else
             try
                fx(e).handle{ (v1,e1) =>
                   if (e1 == null) then
                      retval.complete(v1.nn)
                   else
                      retval.completeExceptionally(e1.nn)
                }
             catch
                case NonFatal(ex) =>
                   ex.addSuppressed(e)
                   retval.completeExceptionally(ex)
        }
        retval

   def adoptCallbackStyle[A](source: (Try[A]=>Unit) => Unit): CompletableFuture[A] =
        val retval = new CompletableFuture[A]
        source{
          case Success(v) => retval.complete(v)
          case Failure(e) => retval.completeExceptionally(e)
        }
        retval

   def spawn[A](op: => CompletableFuture[A]): CompletableFuture[A] =
        val r = new CompletableFuture[A]()
        CompletableFuture.runAsync{()=>
          try
            op.handle{ (v,e) =>
              if (e == null)
                 r.complete(v)
              else
                 r.completeExceptionally(e)
            }
          catch
            case NonFatal(e) =>
               r.completeExceptionally(e)
        }
        r

    def tryCancel[A](op: CompletableFuture[A]): CompletableFuture[Unit] =
        if (op.cancel(true)) then
           CompletableFuture.completedFuture(()).nn
        else
           CompletableFuture.failedFuture(new IllegalStateException("CompletableFuture is not cancelled")).nn

}


