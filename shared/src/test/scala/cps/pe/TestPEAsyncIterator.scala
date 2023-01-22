package cps.pe

import org.junit.{Test,Ignore}
import org.junit.Assert._

import scala.quoted._
import scala.util.Success

import cps._
import cps.stream._

import cps.util.FutureCompleter
import cps.testconfig.given

import scala.concurrent.ExecutionContext.Implicits.global

class TestPEAsyncIterator {

  val N = 10_000

  @Test 
  def peRunIteratorOnAsyncList() = {

    val stream: AsyncList[PureEffect, Int] = asyncStream[AsyncList[PureEffect, Int]] { out =>
      out.emit(0)
      for i <- 1 to N do out.emit(i)
    }

    val ite = stream.iterator
    val compute: PureEffect[Long] = async[PureEffect] {
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

    FutureCompleter(compute.unsafeRunFuture())
  

  }


}