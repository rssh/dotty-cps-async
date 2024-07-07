package gears.async

import cps.*

import scala.concurrent.*
import scala.scalajs.js.{JavaScriptException, Thenable}
import scala.util.*
import scala.util.control.NonFatal
import scalajs.*


/**
 * AsyncMonad
 **/
sealed trait JSAsync[+A] {

  def map[B](f: A => B): JSAsync[B]

  def flatMap[B](f: A => JSAsync[B]): JSAsync[B]

  def mapTry[B](f: Try[A] => B): JSAsync[B] =
    flatMapTry((ta: Try[A]) => JSAsync.Pure(f(ta)))

  def flatMapTry[B](f: Try[A] => JSAsync[B]): JSAsync[B]

  def toPromise: js.Promise[A]

  def toFuture: Future[A]

  def onComplete(f: Try[A] => Unit): Unit

}




object JSAsync {



   case class JSPromiseWrapper[A](val jsPromise: js.Promise[Either[A,JSAsync[A]]]) extends JSAsync[A] {

      override def map[B](f: A=>B): JSAsync[B] = {
        JSPromiseWrapper(
          jsPromise.`then`[Either[B,JSAsync[B]]]( la =>
            la match
              case Left(a) =>
                try
                  Left(f(a))
                catch
                  case NonFatal(ex) =>
                    Right(Error(ex))
              case Right(next) =>
                Right(next.map(f))
          )
        )
      }

      override def flatMap[B](f: A => JSAsync[B]): JSAsync[B] = {
        import scala.concurrent.ExecutionContext.Implicits.global
        JSPromiseWrapper(
           jsPromise.`then`[Either[B,JSAsync[B]]] {
             case Left(a) =>
               evalOp(f(a))
             case Right(lPrev) =>
               // TODO: mb deque of task to prevent quadratic complexity of flatMap chains.
               Right(lPrev.flatMap(f))
           }
        )
      }

      override def flatMapTry[B](f: Try[A] => JSAsync[B]): JSAsync[B] = {
        import scala.concurrent.ExecutionContext.Implicits.global

        JSPromiseWrapper(
           jsPromise.`then`[Either[B,JSAsync[B]]](
             onFulfilled = {
                 case Left(a) =>
                   evalOp(f(Success(a)))
                 case Right(wa) =>
                   wa match
                     case Error(ex) =>
                       evalOp(f(Failure(ex)))
                     case other =>
                       Right(other.flatMapTry(f))
             },
             onRejected = { (e: Any) =>
                   e match
                     case ex: Throwable =>
                       evalOp(f(Failure(ex)))
                     case other =>
                       evalOp(f(Failure(JavaScriptException(other))))
               }
           )
        )
      }

      override def toPromise: js.Promise[A] = {
        jsPromise.`then`[A]({
          case Left(a) => a
          case Right(next) =>
            next.toPromise
        })
      }

      override def toFuture: Future[A] = {
        import scala.concurrent.ExecutionContext.Implicits.global
        jsPromise.toFuture.flatMap({
          case Left(a) => Future successful a
          case Right(next) => next.toFuture
        })
      }

      override def onComplete(f: Try[A] => Unit): Unit = {
        jsPromise.`then`[Unit]({
          case Left(a) => f(Success(a))
          case Right(next) => next.onComplete(f)
        })
      }

   }

   case class ScalaFutureWrapper[A](future: Future[Either[A,JSAsync[A]]]) extends JSAsync[A] {
      import scala.concurrent.ExecutionContext.Implicits.global

      override def map[B](f: A=>B): JSAsync[B] = {
        ScalaFutureWrapper(future.map({
          case Left(a) => evalOp(Pure(f(a)))
          case Right(next) => Right(next.map(f))
        }))
      }

      override def flatMap[B](f: A=>JSAsync[B]) = {
        val next:Future[Either[B,JSAsync[B]]] = future.flatMap{
          case Left(a) =>
            Future successful evalOp[B](f(a))
          case Right(wa) =>
            wa match
              case Pure(a) =>
                Future successful evalOp(f(a))
              case Error(ex) =>
                Future failed ex
              case ScalaFutureWrapper(fw) =>
                fw.flatMap{
                  case Left(a) => Future successful evalOp[B](f(a))
                  case Right(next) => Future successful Right(next.flatMap(f))
                }
              case other =>
                Future successful( Right(other.flatMap(f)) )
        }
        ScalaFutureWrapper(next)
      }


      override def flatMapTry[B](f: Try[A] => JSAsync[B]): JSAsync[B] = {
        val next: Future[Either[B,JSAsync[B]]] = future.flatMap{
          case Left(a) =>
            Future successful evalOp(f(Success(a)))
          case Right(wa) =>
            wa match
              case Pure(a) =>
                Future successful evalOp(f(Success(a)))
              case Error(ex) =>
                Future successful evalOp(f(Failure(ex)))
              case other =>
                Future successful Right(other.flatMapTry(f))
        }
        ScalaFutureWrapper(next)
      }

      override def toPromise: js.Promise[A] = {
        val p = js.Promise[A]((resolve, reject) => {
          future.onComplete({
            case Success(Left(a)) =>
              resolve(a)
            case Success(Right(next)) =>
              resolve(next.toPromise)
            case Failure(ex) =>
              reject(ex)
          })
        })
        p
      }

      override def toFuture: Future[A] = {
        future.flatMap({
          case Left(a) => Future successful a
          case Right(next) => next.toFuture
        })
      }

      override def onComplete(f: Try[A] => Unit): Unit = {
        future.onComplete({
          case Success(Left(a)) => f(Success(a))
          case Success(Right(next)) => next.onComplete(f)
          case Failure(ex) => f(Failure(ex))
        })
      }

   }

   case class Pure[A](val value: A) extends JSAsync[A] {

      override def map[B](f: A=>B): JSAsync[B] = {
        try
          Pure(f(value))
        catch
          case  NonFatal(ex) =>
            Error(ex)
      }

      override def flatMap[B](f: A=>JSAsync[B]): JSAsync[B] = {
        try
          f(value)
        catch
          case NonFatal(ex) => Error(ex)
            ScalaFutureWrapper(Future.failed(ex))
      }

      override def flatMapTry[B](f: Try[A] => JSAsync[B]): JSAsync[B] = {
        try
          f(Success(value))
        catch
          case NonFatal(ex) =>
            Error(ex)
      }

      override def toPromise: js.Promise[A] = {
        js.Promise.resolve(value)
      }

      override def toFuture: Future[A] = {
        Future successful value
      }

      override def onComplete(f: Try[A] => Unit): Unit = {
        f(Success(value))
      }


   }

   object Pure {

     val unit: Pure[Unit] = new Pure(())

   }

   case class Error(ex: Throwable) extends JSAsync[Nothing] {

      override def map[B](f: Nothing=>B): JSAsync[B] = {
        this
      }

      override def flatMap[B](f: Nothing=>JSAsync[B]): JSAsync[B] = {
        this
      }

      override def flatMapTry[B](f: Try[Nothing] => JSAsync[B]): JSAsync[B] = {
        try
          f(Failure(ex))
        catch
          case NonFatal(ex1) =>
            Error(ex1)
      }

      override def toPromise: js.Promise[Nothing] = {
        js.Promise.reject(ex)
      }

      override def toFuture: Future[Nothing] = {
        Future failed ex
      }

      override def onComplete(f: Try[Nothing] => Unit): Unit = {
        f(Failure(ex))
      }

   }



   class JSAsyncCpsMonad(jctx: Async) extends CpsTryContextMonad[JSAsync, Async] {

     override def pure[A](a:A): JSAsync[A] = JSAsync.Pure(a)

     override def map[A,B](fa:JSAsync[A])(f: A => B): JSAsync[B] =
                  fa.map(f)

     override def flatMap[A,B](fa:JSAsync[A])(f: A => JSAsync[B]): JSAsync[B] =
                  fa.flatMap(f)

     override def error[A](e: Throwable): JSAsync[A] =
                  Error(e)

     override def flatMapTry[A,B](fa:JSAsync[A])(f: Try[A] => JSAsync[B]): JSAsync[B] =
                  fa.flatMapTry(f)

     override def applyContext[A](op: Async => JSAsync[A]): JSAsync[A] = {
       try
        op(jctx)
       catch
         case NonFatal(ex) =>
           ScalaFutureWrapper(Future.failed(ex))
     }

   }

   given emptyJSAsyncMonad: JSAsyncCpsMonad = new JSAsyncCpsMonad(Async.empty)

   given CpsMonadConversion[js.Promise, JSAsync] with {

     def apply[T](p: js.Promise[T]): JSAsync[T] =
       JSPromiseWrapper(p.`then`(v => Left(v)))

   }

   given CpsMonadConversion[Future, JSAsync] with {

      def apply[T](f: Future[T]): JSAsync[T] =
        import scala.concurrent.ExecutionContext.Implicits.global
        ScalaFutureWrapper(f.map(Left(_)))

   }

   def eval[A](f: =>A): JSAsync[A] = {
      try
        Pure(f)
      catch
        case NonFatal(ex) =>
          Error(ex)
   }


   def evalOp[T](op: => JSAsync[T]): Either[T, JSAsync[T]] = {
      try
        val r = op
        r match
          case Pure(v) =>
            Left(v)
          case other =>
            Right(other)
      catch
        case NonFatal(ex) =>
          Right(Error(ex))
   }

   def failure(ex: Throwable): JSAsync[Nothing] =
      Error(ex)

   def success[A](a: A): JSAsync[A] = Pure(a)


}


