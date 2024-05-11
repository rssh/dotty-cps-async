package m1

import scala.annotation.experimental
import scala.util.*
import scala.scalajs.*

import cps.*

sealed trait JSAsync[+A]

case class Suspension[A,R](label:Long, body: A => JSAsync[R])

object JSAsync {

  case class JSPromiseWrapper[A](p: js.Promise[Either[A,JSAsync[A]]]) extends JSAsync[A]

  case class Pure[A](a: A) extends JSAsync[A]

  case class Error[A](e: Throwable) extends JSAsync[A]

  case class JSSuspend[A,R](f: () => JSAsync[A]) extends JSAsync[Suspension[A,R]]

  case class JSContinue[A,R](f: A => JSAsync[R]) extends JSAsync[R]


  class MyCpsTryMonad(context: JSAsyncContext) extends CpsTryContextMonad[JSAsync, JSAsyncContext] {

    override def pure[A](a: A): JSAsync[A] = JSAsync.Pure(a)

    override def error[A](e: Throwable): JSAsync[A] = JSAsync.Error(e)

    override def map[A,B](fa: JSAsync[A])(f: A => B): JSAsync[B] = {
      ???
    }

    override def flatMap[A, B](fa: JSAsync[A])(f: A => JSAsync[B]): JSAsync[B] = {
      ???
    }

    override def flatMapTry[A, B](fa: JSAsync[A])(f: Try[A] => JSAsync[B]): JSAsync[B] = {
      ???
    }

    override def applyContext[T](op: JSAsyncContext => JSAsync[T]): JSAsync[T] = ???

  }


}

trait JSAsyncContext extends CpsTryMonadContext[JSAsync] {

  override val monad = new JSAsync.MyCpsTryMonad(this)

}

@experimental
@cps.plugin.annotation.CpsNotChange
given jsAsyncFromDirect(using direct: CpsDirect[JSAsync]): JSAsyncContext =
  direct.context.asInstanceOf[JSAsyncContext]


@experimental
object CustomContext {

  def problematicMethod[A](body: JSAsyncContext => JSAsync[A])(using CpsDirect[JSAsync]): A = {

    val r = await(body(summon[JSAsyncContext]))

    r
  }

}
