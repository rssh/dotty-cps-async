package cps.util

import scala.concurrent.*
import scala.concurrent.duration.*

import java.util.Timer
import java.util.TimerTask


object FutureSleep:

   lazy val timer = new java.util.Timer()

   def apply(duration: Duration):Future[Duration] = {
      val p = Promise[Duration]
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
