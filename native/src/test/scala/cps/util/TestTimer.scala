package cps.util

import scala.collection.mutable
import scala.concurrent.*
import scala.concurrent.duration.*
import scala.concurrent.ExecutionContext.Implicits.global



object TestTimer:

   
   class Sleeper(val wakeTime: Long, val runnable: Runnable, @volatile var cancelled:Boolean)

   type CancelToken = Sleeper

   private val queue = mutable.PriorityQueue[Sleeper]()(using Ordering.by[Sleeper,Long](_.wakeTime))
   @volatile var shutdownFlag = false

   
   val sleeperThread = new Thread {
      override def run(): Unit = {
         val executionContext = summon[ExecutionContext]
         while(!shutdownFlag) {
            val now = System.currentTimeMillis()
            val next = TestTimer.synchronized(queue.headOption)
            next match
               case Some(sleeper) =>
                  if (sleeper.wakeTime <= now) then
                     TestTimer.synchronized(queue.dequeue())
                     if (!sleeper.cancelled) then
                        executionContext.execute(sleeper.runnable)
                  else
                     val sleepTime = sleeper.wakeTime - now
                     TestTimer.synchronized {
                        TestTimer.wait(sleepTime)
                     }
               case _ =>
                  val sleepTime = 1000L
                  TestTimer.synchronized {
                     TestTimer.wait(sleepTime)
                  }
         }
      }
   }
   sleeperThread.setDaemon(true)
   sleeperThread.start()

   def delay(duration: FiniteDuration):Future[Duration] = {
      val p = Promise[Duration]()
      schedule(duration)(()=>p.success(duration))
      p.future
   }

   def schedule(duration: FiniteDuration)(f: =>Unit): CancelToken = {
      val wakeTime = System.currentTimeMillis() + duration.toMillis
      val sleeper = Sleeper(wakeTime, () => f, false)
      TestTimer.synchronized {
         queue.enqueue(sleeper)
         TestTimer.notify()
      }
      sleeper
   }

   def cancel(ct: CancelToken): Unit = {
     ct.cancelled = true
   }

   def shutdown(): Unit = {
      shutdownFlag = true
      TestTimer.synchronized {
         TestTimer.notify()
      }
   }
