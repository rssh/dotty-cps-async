package cps.stream

import org.junit.{Test,Ignore}
import org.junit.Assert._

import scala.concurrent.*
import scala.concurrent.ExecutionContext.Implicits.global


import cps.*
import cps.stream.*
import cps.monads.{*, given}

import cps.testconfig.given
import cps.util.FutureCompleter

class TestAsyncListIterator {

  val N = 10_000

  @Test 
  def runIteratorOnAsyncList() = {

    val stream: AsyncList[Future, Int] = asyncStream[AsyncList[Future, Int]] { out =>
      out.emit(0)
      for i <- 1 to N do out.emit(i)
    }

    val ite = stream.iterator
    val compute: Future[Long] = async[Future] {
      var n = await(ite.next)
      var res: Long = 0
      var count: Int = 0
      while (n.nonEmpty) {
        val v = n.get
        res += v
        count += 1
        n = await(ite.next)
      }
      assert(count == N+1)
      res
    }

    FutureCompleter(compute)
  }

}