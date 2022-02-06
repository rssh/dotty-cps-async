package cps.util

import scala.concurrent.*
import scala.concurrent.duration.*

import java.util.Timer
import java.util.TimerTask


object TestTimer:

   lazy val timer = new java.util.Timer()

   type CancelToken = java.util.TimerTask

   def delay(duration: FiniteDuration):Future[FiniteDuration] = {
      val p = Promise[FiniteDuration]
      val timerTask = new TimerTask {
          
          override def run(): Unit = {
             p.trySuccess(duration)
          }

          override def cancel(): Boolean = {
             val retval = super.cancel()
             if (retval) {
               p.tryFailure(new CancellationException())
             }
             retval
         }
      }
      timer.schedule(timerTask, duration.toMillis)
      p.future
   }

   def schedule(duration: FiniteDuration)(f: =>Unit): CancelToken = {
     val timerTask = new TimerTask {
       override def run(): Unit = {
         try{
           f
         }catch{
           case ex: Throwable =>
            ex.printStackTrace()
            throw ex
         }
       }
     }
     timer.schedule(timerTask, duration.toMillis)
     timerTask
   }

   def cancel(ct: CancelToken): Unit = {
    ct.cancel()
  }

