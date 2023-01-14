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

  val N = 10_000_000

  @Test 
  @Ignore
  def runIteratorOnAsyncList() = {

    val stream: AsyncList[PureEffect, Int] = asyncStream[AsyncList[PureEffect, Int]] { out =>
      println("begin generator")
      out.emit(0)
      println("plop")
      for i <- 1 to N do out.emit(i)
    }

    println("before for loop")
    val ite = stream.iterator
    val compute: PureEffect[Long] = async[PureEffect] {
      println("begin sink, before calling ite.next")
      var n = await(ite.next)
      println(s"after ite.next, n=$n")
      var res: Long = 0
      println("before while")
      var count: Int = 0
      while (n.nonEmpty) {
        res += n.get
        count += 1
        println(s"receive res $res")
        n = await(ite.next)
      }

      println(s"pe.asuncList:count=$count")
      assert(count == N)
      res
    }

    FutureCompleter(compute.unsafeRunFuture())
  

  }


}