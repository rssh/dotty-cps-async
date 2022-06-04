package futureScope.util

import java.util.concurrent.Executors
import java.util.concurrent.TimeUnit


import scala.concurrent.*
import scala.concurrent.duration.*
import scala.util.*
import scala.util.control.NonFatal

import futureScope.*

object TimeOperations {


  def now(): Duration =
    System.currentTimeMillis().millis

  /**
   * reurn CancellableFuture not bound to any context.
   **/
  def waiting[A](duration: FiniteDuration)(f: =>A): CancellableFuture[A] = {
    val p = Promise[A]()
    val setP: Runnable = () => {
      try
        p.success(f)
      catch
        case NonFatal(ex) =>
          p.failure(ex)    
    }
    val jf = se.schedule(setP, duration.toMillis, TimeUnit.MILLISECONDS).nn
    DelegatedCancellableFuture(p.future, reason => { 
                         jf.cancel(true)
                         if (p.tryFailure(reason)) then
                            CancellationResult.Cancelled
                         else
                            CancellationResult.AlreadyFinished 
                        })
  }
 
  private[this] lazy val se = {
    // TODO: resolver set scheduler or get parameters from config
    Executors.newScheduledThreadPool(1).nn 
  }
 

}