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
     * Create a future which will return true or false
     * <code> fa </code> has no completed duriong <code> duration </code>
     **/
    def delayedComplete(duration: FiniteDuration)(a: =>Try[A])(using ExecutionContext, ScheduledExecutorService): Future[A] =          
      val p = Promise[A]()
      val runTimeout: Runnable = ()=>{
        try
          a match
            case Failure(ex) => p.tryFailure(ex)
            case Success(v) => p.trySuccess(v)
        catch
          case NonFatal(ex) =>
            p.tryFailure(ex)
      }
      summon[ScheduledExecutorService].schedule(runTimeout ,duration.toMillis.toLong, TimeUnit.MILLISECONDS )
      p.completeWith(fa)
      p.future



    /**
     * Create a future which will return or value of original future <code> fa </code> or raise a timeout exception, if
     * <code> fa </code> has no completed duriong <code> duration </code>
     **/
    def withTimeout(duration: FiniteDuration)(using ExecutionContext, ScheduledExecutorService): Future[A] =
      delayedComplete(duration)( Try(throw new TimeoutException()) )

    /**
     * Create a future which will return true or false
     * <code> fa </code> has no completed duriong <code> duration </code>
     **/
    def withDelay(duration: FiniteDuration)(a: =>A)(using ExecutionContext, ScheduledExecutorService): Future[A] =          
      delayedComplete(duration)( Try(a) )



/**
 * Timed await -- await for future or throw TimeoutException after <code> duration </code>
 **/    
transparent inline def timedAwait[A](fa: Future[A], duration: FiniteDuration)(using CpsMonadContext[Future], ExecutionContextProvider, ScheduledExecutorService): A =
  given ExecutionContext = summon[ExecutionContextProvider].executionContext
  await(fa.withTimeout(duration))

transparent inline def timedReady[A](fa: Future[A], duration: FiniteDuration)(using CpsMonadContext[Future], ExecutionContextProvider, ScheduledExecutorService): Boolean =
  given ExecutionContext = summon[ExecutionContextProvider].executionContext
  await(fa.map(_ => true).withDelay(duration)(false))
  
