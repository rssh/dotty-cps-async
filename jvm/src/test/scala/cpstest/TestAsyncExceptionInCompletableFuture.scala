package cpstest

import org.junit.{Ignore, Test}
import org.junit.Assert.*

import java.util.concurrent.CompletableFuture
import scala.concurrent.*
import scala.concurrent.duration.*
import scala.util.*
import scala.util.control.*
import scala.concurrent.ExecutionContext.Implicits.global

import cps.*
import cps.monads.{*, given}

class TestAsyncExceptionInCompletableFuture {

  object X {

    def completableFutureMethod(): CompletableFuture[Int] = {
      val cf = new CompletableFuture[Int]()
      cf.completeExceptionally(new IllegalStateException("test exception"))
      cf
    }

  }

  @Test
  def testExceptinInCompletableFuture(): Unit = {
    val f = async[Future] {
      try {
        val x = X.completableFutureMethod().await
        x
      } catch {
        case ex: IllegalStateException =>
          -1
        case NonFatal(ex) =>
          println(s"unexpected exception: $ex")
          throw ex
      }
    }
    val x = Await.ready(f, 1.second)
    assert(f.value.get.get == -1)
  }

}
