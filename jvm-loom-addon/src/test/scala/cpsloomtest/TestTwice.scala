package cpsloomtest

import scala.concurrent.*
import scala.concurrent.duration.*
import scala.concurrent.ExecutionContext.Implicits.global
import cps.*
import cps.monads.{*, given}
import org.junit.Test


class TestTwice {

  def twice[A](f: A=>A): A=>A =
    a => f(f(a))

  @Test
  def testAwaitInTwiceArg(): Unit = {

    val f = async[Future]{
      twice{ (x: Int) =>
        await(Future.successful(x+1))
      }(1)
    }
    val r = Await.result(f, 1.second)
    assert(r == 3)
  }

}
