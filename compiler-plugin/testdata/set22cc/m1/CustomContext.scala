package m1

import scala.annotation.experimental
import scala.util.*
import scala.scalajs.*

import cps.*
import cps.plugin.annotation.*

sealed trait JSAsync[+A]

case class Suspension[A,R](label:Long, body: A => JSAsync[R])

object JSAsync {


  class MyCpsTryMonad(context: JSAsyncContext) extends CpsTryContextMonad[JSAsync, JSAsyncContext] {

    override def pure[A](a: A): JSAsync[A] = ???

    override def error[A](e: Throwable): JSAsync[A] = ???

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
//@CpsNotChange
given jsAsyncFromDirect(using direct: CpsDirect[JSAsync]): JSAsyncContext =
  direct.context.asInstanceOf[JSAsyncContext]


@experimental
@CpsDebugLevel(20)
object CustomContext {


  def problematicMethod[A](body: JSAsyncContext => JSAsync[A])(using CpsDirect[JSAsync]): A = {
    val r = await(body(summon[JSAsyncContext]))
    //val r = await(r0)
    r
  }

}
