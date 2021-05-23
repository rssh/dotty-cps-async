package cps.monads.jsfuture

import cps._

import scala.language.implicitConversions

import scalajs.*
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
import scala.concurrent.Future
import scala.util.*
import scala.util.control.NonFatal


object JSFutureExecutor:

    def flattenFutureComplete[T](future: Future[T],
           resolve: js.Function1[T, ?],
           reject: js.Function1[scala.Any, ?] ): Unit =
      future.onComplete{
         case Success(v) =>
            v match
               case vf: Future[_] =>
                      flattenFutureComplete(vf.asInstanceOf[Future[T]], resolve, reject)
               //  can't publish:  bug in dotty
               //case thenable: js.Promise[_] =>
               //       val castedPromise = thenable.asInstanceOf[js.Promise[T]]
               //       castedPromise.`then`(x => {resolve(x);()} , (e:scala.Any) => {reject(e);()} )
               case other =>
                      // TODO: check exception
                      resolve(v)
          case Failure(e) =>
                      reject(e)
      }
      
                          



class JSFuture[T](
     val executorOrUndef: js.UndefOr[js.Function2[js.Function1[T | js.Thenable[T], _], js.Function1[scala.Any, _], _]],
     val futureOrUndef: js.UndefOr[Future[T]]) extends js.Promise[T](
                       if (js.isUndefined(executorOrUndef)) then
                          if (js.isUndefined(futureOrUndef)) then 
                             throw js.JavaScriptException(js.Error("Invalid call of JSFuture constructor"))
                          else
                             (resolve, reject) => JSFutureExecutor.flattenFutureComplete(futureOrUndef.get, resolve, reject)
                       else
                          executorOrUndef.get
                  ) {

   lazy val future: Future[T] = if (futureOrUndef.isDefined) then futureOrUndef.get else this.toFuture
 
   def map[S](f: T=>S): JSFuture[S] =
        new JSFuture[S](js.undefined, future.map(f))
        

   def flatMap[S](f: T=> JSFuture[S]): JSFuture[S] =
     new JSFuture[S](js.undefined, future.flatMap{ x => f(x).future })

   def mapTry[S](f: Try[T]=>S): JSFuture[S] =
     new JSFuture[S](js.undefined, 
                    future.transform{ x =>
                            try 
                              Success(f(x))
                            catch 
                              case NonFatal(ex) => Failure(ex)
                    }
         )

   def flatMapTry[S](f: Try[T]=>JSFuture[S]): JSFuture[S] =
     val newFuture = future.transformWith{ x => f(x).future }
     new JSFuture[S](js.undefined, newFuture)

   // can't override due to dotty bug:
   //    https://github.com/lampepfl/dotty/issues/12572
   //  so, now accept call of costructr as in defailt implementation of promise.
   //
   //override def `then`[S](
   //   onFulfilled: js.Function1[T, S | js.Thenable[S]],
   //   onRejected: js.UndefOr[js.Function1[scala.Any, S | js.Thenable[S]]] = js.undefined): js.Promise[S] =
   //     js.Promise( (resolve, reject) =>
   //        JSFutureExecutor.flattenFutureComplete(future, 
   //                            x => resolve(onFulfilled(x)), 
   //                            e => if (!onRejected.isEmpty) {
   //                                   resolve(onRejected.get.apply(e))
   //                                 } else {
   //                                   reject(e)
   //                                 }
   //        )
   //     )

   //override def `then`[S >: T](
   //   onFulfilled: Unit,
   //   onRejected: js.UndefOr[js.Function1[scala.Any, S | js.Thenable[S]]]): js.Promise[S] = 
   //     `then`[S]( x => x , onRejected)

   // override def `catch`[S >: T](
   //   onRejected: js.UndefOr[js.Function1[scala.Any, S | js.Thenable[S]]] = js.undefined): js.Promise[S] = 
   //     `then`[S]( x => x , onRejected )
       
   

}


given JSFutureCpsMonad: CpsTryMonad[JSFuture] with


   def pure[A](a:A): JSFuture[A] = new JSFuture[A](js.undefined, Future successful a)

   def map[A,B](fa: JSFuture[A])(f: A=>B): JSFuture[B] =
        fa.map(f)

   def flatMap[A,B](fa: JSFuture[A])(f: A=> JSFuture[B]): JSFuture[B] =
        fa.flatMap(f)

   def error[A](ex:Throwable): JSFuture[A] = new JSFuture[A](js.undefined, Future failed ex)

   override def mapTry[A,B](fa: JSFuture[A])(f: Try[A]=>B): JSFuture[B] =
        fa.mapTry(f)

   def flatMapTry[A,B](fa: JSFuture[A])(f: Try[A]=> JSFuture[B]): JSFuture[B] =
        fa.flatMapTry(f)


given CpsMonadConversion[Future,JSFuture] with
   def apply[T](ft:Future[T]): JSFuture[T] =
         new JSFuture(js.undefined, ft)

given CpsMonadConversion[JSFuture, Future] with
   def apply[T](ft:JSFuture[T]): Future[T] = ft.future

given CpsMonadConversion[js.Promise, JSFuture] with
   def apply[T](ft:js.Promise[T]): JSFuture[T] = 
         new JSFuture(js.undefined, ft.toFuture)

