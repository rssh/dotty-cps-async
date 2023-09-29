package cpsloomtest

import org.junit.{Test,Ignore}
import org.junit.Assert.*
import scala.annotation.experimental
import scala.concurrent.*
import scala.concurrent.duration.*
import scala.concurrent.ExecutionContext.Implicits.global

import cps.*
import cps.monads.{*,given}
import cps.plugin.annotation.CpsDebugLevel


@experimental
class TestIncrMin {

  def incr(x: Int): Future[Int] = async[Future] {
    x + 1
  }

  def callIncr(x: Int)(using CpsDirect[Future]): Int = {
    await(incr(x))
  }


  @Test def testIncr() = {
    val f = async[Future] {
        val x = callIncr(1)
        val y = callIncr(2)
        x + y
    }
    val r = Await.result(f, 1.second)
    assert(r == 5)
  }

}