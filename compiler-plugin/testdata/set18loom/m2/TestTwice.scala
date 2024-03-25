package cpsloomtest

import scala.annotation.experimental
import scala.concurrent.*
import scala.concurrent.duration.*
import scala.concurrent.ExecutionContext.Implicits.global
import cps.*
import cps.monads.{*, given}
import org.junit.Test

@experimental
class TestTwice {

  def twice[A](f: A=>A): A=>A =
    a => f(f(a))

  def callTwice(x:Int)(using CpsDirect[Future]): Int = {
    twice{ (x: Int) =>
      await(Future.successful(x+1))
    }(x)
  }

  @Test
  def testAwaitInTwiceArg(): Unit = {
    val f = async[Future]{
      callTwice(1)
    }
    val r = Await.result(f, 1.second)
    assert(r == 3)
  }

}
