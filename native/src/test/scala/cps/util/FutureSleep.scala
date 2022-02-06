package cps.util

import scala.concurrent.*
import scala.concurrent.duration.*
import scala.concurrent.ExecutionContext.Implicits.global

import scala.scalanative.loop.*


object FutureSleep:

   def apply(duration: FiniteDuration):Future[Duration] = {
      Timer.delay(duration).map(_ => duration)
   }

