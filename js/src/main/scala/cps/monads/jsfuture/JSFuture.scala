package cps.monads.jsfuture

import cps._

import scala.language.implicitConversions

import scalajs.*
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
import scala.concurrent.Future
import scala.util.*
import scala.util.control.NonFatal


class JSFuture[T](val callbacks: JSFuture.Callbacks[T], val future: Future[T]) extends js.Promise[T](
                                                     (resolve, reject) => callbacks.add(resolve, reject))
{
   future.onComplete(callbacks.fire(_))

   def map[S](f: T=>S): JSFuture[S] =
     new JSFuture[S](new JSFuture.Callbacks(), future.map(f))

   def flatMap[S](f: T=> JSFuture[S]): JSFuture[S] =
     val newFuture = future.flatMap{ x => f(x).future }
     new JSFuture[S](new JSFuture.Callbacks(), newFuture)

   def mapTry[S](f: Try[T]=>S): JSFuture[S] =
     new JSFuture[S](new JSFuture.Callbacks(), 
                    future.transform{ x =>
                            try 
                              Success(f(x))
                            catch 
                              case NonFatal(ex) => Failure(ex)
                    }
         )

   def flatMapTry[S](f: Try[T]=>JSFuture[S]): JSFuture[S] =
     val newFuture = future.transformWith{ x => f(x).future }
     new JSFuture[S](new JSFuture.Callbacks(), newFuture)

}

object JSFuture{

  class Callbacks[T](
     var onResolve: List[js.Function1[T| js.Thenable[T],?]] = List.empty,
     var onReject: List[js.Function1[Any, ?]] = List.empty
  ) {

    def add( onResolve: js.Function1[T | js.Thenable[T], ?], onReject: js.Function1[Any, ?] ) =
      this.onResolve = onResolve::this.onResolve
      this.onReject = onReject::this.onReject

    def merge(other: Callbacks[T]): Unit =
      this.onResolve = this.onResolve ++ other.onResolve
      this.onReject = this.onReject ++ other.onReject

    def fire(r: Try[T]): Unit =
       r match
        case Success(v) => 
                   // TODO: think about exception in listener
                   onResolve.foreach{ _.apply(v) }
        case Failure(ex) =>
               ex match
                  case js.JavaScriptException(v) =>
                      onReject.foreach{  _.apply(v) }
                  case _ =>
                      onReject.foreach{  _.apply(ex) }
  }

  def newCallbacks[T](): Callbacks[T] = new Callbacks[T]()

}


given JSFutureCpsMonad: CpsTryMonad[JSFuture] with


   def pure[A](a:A): JSFuture[A] = new JSFuture[A](new JSFuture.Callbacks(), Future successful a)

   def map[A,B](fa: JSFuture[A])(f: A=>B): JSFuture[B] =
        fa.map(f)

   def flatMap[A,B](fa: JSFuture[A])(f: A=> JSFuture[B]): JSFuture[B] =
        fa.flatMap(f)

   def error[A](ex:Throwable): JSFuture[A] = new JSFuture[A](new JSFuture.Callbacks(), Future failed ex)

   override def mapTry[A,B](fa: JSFuture[A])(f: Try[A]=>B): JSFuture[B] =
        fa.mapTry(f)

   def flatMapTry[A,B](fa: JSFuture[A])(f: Try[A]=> JSFuture[B]): JSFuture[B] =
        fa.flatMapTry(f)



