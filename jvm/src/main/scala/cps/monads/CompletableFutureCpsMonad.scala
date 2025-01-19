package cps.monads

import cps.*

import java.util.concurrent.{CompletableFuture, CompletionException}
import scala.concurrent.Future
import scala.util.{Failure, NotGiven, Success, Try}
import scala.util.control.NonFatal

given CompletableFutureCpsMonad: CpsSchedulingMonad[CompletableFuture] with CpsTryMonadInstanceContext[CompletableFuture] with {

  def pure[T](t: T): CompletableFuture[T] =
    CompletableFuture.completedFuture(t).nn

  def map[A, B](fa: CompletableFuture[A])(f: A => B): CompletableFuture[B] =
    fa.thenApplyAsync(a => f(a.nn)).nn // TODO:  dotty bug report: Function to java.util.Function

  def flatMap[A, B](fa: CompletableFuture[A])(f: A => CompletableFuture[B]): CompletableFuture[B] =
    fa.thenComposeAsync(a => f(a.nn)).nn

  def error[A](e: Throwable): CompletableFuture[A] =
    CompletableFuture.failedFuture(e).nn

  override def mapTry[A, B](fa: CompletableFuture[A])(f: Try[A] => B): CompletableFuture[B] =
    fa.handle { (v, e) =>
      if (e == null) then f(Success(v.nn))
      else f(Failure(unwrapCompletableException(e.nn)))
    }.nn
      .toCompletableFuture
      .nn

  override def flatMapTry[A, B](fa: CompletableFuture[A])(f: Try[A] => CompletableFuture[B]): CompletableFuture[B] =
    val retval = new CompletableFuture[B]
    fa.handle { (v, e) =>
      if (e == null) then
        try
          f(Success(v.nn)).handle { (v1, e1) =>
            if (e1 == null) then retval.complete(v1.nn)
            else retval.completeExceptionally(unwrapCompletableException(e1))
          }
        catch
          case NonFatal(ex) =>
            retval.completeExceptionally(unwrapCompletableException(ex))
      else
        try
          f(Failure(unwrapCompletableException(e.nn))).handle { (v1, e1) =>
            if (e1 == null) then retval.complete(v1.nn)
            else retval.completeExceptionally(unwrapCompletableException(e1.nn))
          }
        catch
          case NonFatal(ex) =>
            retval.completeExceptionally(ex)
    }
    retval

  override def restore[A](fa: CompletableFuture[A])(fx: Throwable => CompletableFuture[A]): CompletableFuture[A] =
    val retval = new CompletableFuture[A]
    fa.handle { (v, e) =>
      if (e == null) then retval.complete(v.nn)
      else
        try
          fx(unwrapCompletableException(e)).handle { (v1, e1) =>
            if (e1 == null) then retval.complete(v1.nn)
            else retval.completeExceptionally(unwrapCompletableException(e1.nn))
          }
        catch
          case NonFatal(ex) =>
            ex.addSuppressed(e)
            retval.completeExceptionally(ex)
    }
    retval

  def adoptCallbackStyle[A](source: (Try[A] => Unit) => Unit): CompletableFuture[A] =
    val retval = new CompletableFuture[A]
    source {
      case Success(v) => retval.complete(v)
      case Failure(e) => retval.completeExceptionally(e)
    }
    retval

  def spawn[A](op: => CompletableFuture[A]): CompletableFuture[A] =
    val r = new CompletableFuture[A]()
    CompletableFuture.runAsync { () =>
      try
        op.handle { (v, e) =>
          if (e == null)
            r.complete(v)
          else
            r.completeExceptionally(unwrapCompletableException(e))
        }
      catch
        case NonFatal(e) =>
          r.completeExceptionally(e)
    }
    r

  def tryCancel[A](op: CompletableFuture[A]): CompletableFuture[Unit] =
    if (op.cancel(true)) then CompletableFuture.completedFuture(()).nn
    else CompletableFuture.failedFuture(new IllegalStateException("CompletableFuture is not cancelled")).nn

  private def unwrapCompletableException(ex: Throwable): Throwable =
    if (ex.isInstanceOf[CompletionException] && ex.getCause() != null) then ex.getCause().nn
    else ex.nn

  given fromCompletableFutureConversion[G[_], T](using CpsAsyncMonad[G], CpsMonadContext[G]): CpsMonadConversion[
    CompletableFuture,
    G
  ] with

    def apply[T](ft: CompletableFuture[T]): G[T] =
      summon[CpsAsyncMonad[G]].adoptCallbackStyle(listener =>
        val _unused = ft.whenComplete((v, e) =>
          if (e == null) then listener(Success(v))
          else if (e.isInstanceOf[CompletionException] && e.getCause() != null) then listener(Failure(e.getCause()))
          else listener(Failure(e))
        )
      )

}
