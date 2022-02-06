package cps.util

import scala.concurrent.*
import scala.concurrent.duration.*
import scala.concurrent.ExecutionContext.Implicits.global

import scala.scalanative.loop.*


object TestTimer:

   type CancelToken = Timer

   def delay(duration: FiniteDuration):Future[Duration] = {
      Timer.delay(duration).map(_ => duration)
   }

   def schedule(duration: FiniteDuration)(f: =>Unit): CancelToken = {
      Timer.timeout(duration)(()=>f)
   }

   def cancel(ct: CancelToken): Unit = {
     ct.clear()
   }

