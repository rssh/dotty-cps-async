package cpstest

import cps.*
import cps.monads.{given,*}
import cps.util.FutureCompleter

import org.junit.{Test,Ignore}
import org.junit.Assert.*

import java.util.concurrent.ScheduledExecutorService
import java.util.concurrent.Executors
import scala.concurrent.*
import scala.concurrent.duration.*
import scala.util.*




class TestTimedAwait:

  import scala.concurrent.ExecutionContext.Implicits.global
  
  given scheduler: ScheduledExecutorService = Executors.newScheduledThreadPool(1).nn

  @Test def timedAwaitTimeout() = 
    var fa = async[Future] {
      val p = Promise[Int]()
      val r = timedAwait(p.future, 100.millis)
      r
    }
    val fr = fa.transform{
      case Success(x) => Failure(new IllegalStateException("This future should not be successes"))
      case Failure(ex) =>
        if (ex.isInstanceOf[TimeoutException]) {
          Success(true)
        } else {
          Failure(new IllegalStateException(s"Exception is not a TimeoutException but ${ex}"))
        }
    }
    FutureCompleter(fr)

  @Test def timedAwaitNoTimeout() = 
    var fa = async[Future] {
      val p = Promise[Int]()
      p.success(10)
      val r = timedAwait(p.future, 100.millis)
      assert(r == 10)
      r
    }
    FutureCompleter(fa)