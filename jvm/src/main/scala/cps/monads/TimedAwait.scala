package cps.monads

import cps._
import scala.concurrent._
import scala.concurrent.duration._
import scala.quoted._
import scala.util._
import scala.util.control._
import java.util.concurrent.ScheduledExecutorService
import java.util.concurrent.TimeUnit


extension[A] (fa: Future[A])    

    /**
     * Create a future which will return or value of original future <code> fa </code> or raise a timeout exception, if
     * <code> fa </code> has no completed duriong <code> duration </code>
     **/
    def withTimeout(duration: FiniteDuration)(using ExecutionContext, ScheduledExecutorService): Future[A] =
          val p = Promise[A]()
          val runTimeout: Runnable = ()=>p.tryFailure(new TimeoutException())
          summon[ScheduledExecutorService].schedule(runTimeout ,duration.toMillis.toLong, TimeUnit.MILLISECONDS )
          p.completeWith(fa)
          p.future

/**
 * Timed await -- await for future or throw TimeoutException after <code> duration </code>
 **/    
transparent inline def timedAwait[A](fa: Future[A], duration: FiniteDuration)(using FutureContext, ScheduledExecutorService): A =
  given ExecutionContext = summon[FutureContext].executionContext
  await(fa.withTimeout(duration))
