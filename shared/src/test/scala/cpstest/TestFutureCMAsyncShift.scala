package cpstest

import org.junit.Test
import org.junit.Assert.*

import scala.concurrent.*
import scala.concurrent.duration.*
import cps.*
import cps.monads.{*, given}

import java.util.concurrent.atomic.AtomicInteger

import cps.util.FutureCompleter

class TestFutureCMAsyncShift {

  import scala.concurrent.ExecutionContext.Implicits.global

  var q = AtomicInteger(0)


  @Test
  def testFutureApply() = {
    val cr = async[Future] {
      val fr = Future.apply{
        val x = q.incrementAndGet()
        await(Future successful x)
      }
      val r = await(fr)
      assert(r==1)
      r
    }

    FutureCompleter(cr)
  }

}
