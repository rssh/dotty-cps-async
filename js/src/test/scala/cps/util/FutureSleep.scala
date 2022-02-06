package cps.util

import scala.concurrent.*
import scala.concurrent.duration.*

import java.util.Timer
import java.util.TimerTask


object FutureSleep:

   def apply(duration: FiniteDuration):Future[FiniteDuration] = {
      TestTimer.delay(duration)
   }
