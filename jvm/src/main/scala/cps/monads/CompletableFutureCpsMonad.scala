package cps.monads

import cps._
import java.util.concurrent.CompletableFuture
import scala.util.Try
import scala.util.Failure
import scala.util.Success


given CompletableFutureCpsMonad as CpsSchedulingMonad[CompletableFuture] {

   def pure[T](t:T):CompletableFuture[T] =
         CompletableFuture.completedFuture(t)

   def map[A,B](fa:CompletableFuture[A])(f: A=>B):CompletableFuture[B] =
        fa.thenApplyAsync(a => f(a))  // TODO:  dotty bug report: Function to java.util.Function

   def flatMap[A,B](fa:CompletableFuture[A])(f: A=> CompletableFuture[B]):CompletableFuture[B] =
        fa.thenComposeAsync(a => f(a))

   def error[A](e: Throwable): CompletableFuture[A] =
        val retval = new CompletableFuture[A]()
        retval.completeExceptionally(e)
        retval

   def restore[A](fa: CompletableFuture[A])(fx:Throwable => CompletableFuture[A]): CompletableFuture[A] =
        val retval = new CompletableFuture[A]
        fa.handle{ (v,e) => 
          if (e eq null) then
             retval.complete(v)
          else
             fx(e).handle{ (v1,e1) =>
                if (e1 eq null) then
                   retval.complete(v1)
                else
                   retval.completeExceptionally(e1)
             }
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
          op.handle{ (v,e) =>
            if (e eq null) 
               r.complete(v)
            else
               r.completeExceptionally(e)
          }
        }
        r

}


