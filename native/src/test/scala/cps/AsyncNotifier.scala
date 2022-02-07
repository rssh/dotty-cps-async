package cps

import scala.concurrent.*
import scala.concurrent.duration.*
import scala.util.*
import java.util.concurrent.atomic.AtomicReference

import cps.util.TestTimer
import scala.concurrent.ExecutionContext.Implicits.global


class AsyncNotifier {

    val currentPromise: AtomicReference[Promise[Boolean]] = AtomicReference(Promise[Boolean]())

    def doNotifyAll(): Unit = {
      var done = false
      while(!done) {
        val p = currentPromise.getAndSet(Promise[Boolean]())
        if (p.trySuccess(true)) {
           done = true 
        }
      }
    }

    def timedFiniteWait(duration: FiniteDuration):Future[Unit] = {
      val p = Promise[Unit]()
      currentPromise.get.future.onComplete{ r =>
        if (!p.isCompleted) {
          r match
            case Success(v) => p.trySuccess(())
            case Failure(ex) => p.tryFailure(ex)
        }
      }
      TestTimer.schedule(duration){
        if (!p.isCompleted) {
          p.tryFailure(new TimeoutException())
        } 
      }
      p.future
    }

}