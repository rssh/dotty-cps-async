package cps.util

import scala.concurrent.*
import scala.concurrent.duration.*
import scala.concurrent.ExecutionContext.Implicits.global



object FutureSleep:

   def apply(duration: FiniteDuration):Future[Duration] = {
      TestTimer.delay(duration)
   }

