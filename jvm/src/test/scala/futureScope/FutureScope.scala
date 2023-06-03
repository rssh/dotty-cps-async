package futureScope

import cps.plugin.annotation.CpsNotChange

import scala.concurrent.*
import scala.concurrent.duration.*
//import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.*

import cps.*
import cps.monads.{*,given}
import cps.util.*
import cps.testconfig.given

import java.util.concurrent.CancellationException

import org.junit.{Test,Ignore}
import org.junit.Assert._


class FutureScope(ec: ExecutionContext) extends CpsMonadContextProvider[Future] {
  
   override type Context = FutureScopeContext

   override def  contextualize[A](m: CpsTryMonad[Future], fa: Context => Future[A]): Future[A] = {
      val fsc = new FutureScopeContext(m, ec)
      fsc.run(fa)
   }

   class Child(ctx: FutureScopeContext) extends CpsMonadContextProvider[Future] {
 
      override type Context = FutureScopeContext
      
      override def  contextualize[A](m:CpsTryMonad[Future], fa: Context => Future[A]): Future[A] = {
         ctx.spawn_async(fa)
      }

   }
   
   def child(ctx:FutureScopeContext): Child = Child(ctx)

}

object FutureScope {

   /**
   * Spaen computation in current scope
   * This computation is executed in the child wit carrent scope context and will be cancelled on finish of context owner.
   **/
   @CpsNotChange
   def spawn[A](using fsc:FutureScopeContext)(f: FutureScopeContext ?=> A, executionContext: ExecutionContext = fsc.executionContext): CancellableFuture[A] =
      summon[FutureScopeContext].spawn(f,executionContext)

   /**
    * Async version of spawn
    **/
   @CpsNotChange
   def spawnAsync[A](using fsc:FutureScopeContext)(f: FutureScopeContext ?=> Future[A], executionContext: ExecutionContext = fsc.executionContext ): CancellableFuture[A] =
      summon[FutureScopeContext].spawnAsync(f, executionContext)

   /**
    * Async version of spawn with accept plain function. Need for automatic transclaing
    **/
   @CpsNotChange
   def spawn_async[A](using fsc:FutureScopeContext)(f: FutureScopeContext => Future[A], executionContext: ExecutionContext = fsc.executionContext ): CancellableFuture[A] =
      summon[FutureScopeContext].spawn_async(f, executionContext)   

   /**
    * Spawn delay - return a future which can be cancelled
    **/
   @CpsNotChange
   def spawnDelay(using FutureScopeContext)(duration: FiniteDuration):Future[FiniteDuration] =
      summon[FutureScopeContext].spawnDelay(duration)

   @CpsNotChange
   def spawnTimeout(using FutureScopeContext)(duration: FiniteDuration): CancellableFuture[Nothing] =
      summon[FutureScopeContext].spawnTimeout(duration)

   @CpsNotChange
   transparent inline def join()(using FutureScopeContext): Unit =
      summon[FutureScopeContext].join()

   @CpsNotChange
   def cancel()(using FutureScopeContext): CancellationResult =
      summon[FutureScopeContext].cancel(ScopeCancellationException("cancel"))

   @CpsNotChange
   def cancel(ex: ScopeCancellationException)(using FutureScopeContext) =
      summon[FutureScopeContext].cancel(ex)   


   transparent inline def timedAwait[A](fa: Future[A], duration: FiniteDuration)(using fsc:FutureScopeContext):A =
      given ExecutionContext = fsc.executionContext
      await(fsc.timedAwaitAsync(fa,duration))
      
   transparent inline def timedAwaitCompleted[A](fa: Future[A], duration: FiniteDuration)(using fsc:FutureScopeContext) =
      given ExecutionContext = fsc.executionContext
      await(fsc.timedAwaitCompletedAsync(fa,duration))
    
   def isFinished(using FutureScopeContext): Boolean = {
      summon[FutureScopeContext].isFinished
   }
   
   def isActive(using FutureScopeContext): Boolean = {
      summon[FutureScopeContext].isActive
   }

   


   /*   
   def noEscalate[A]( a: =>A)(using FutureScopeContext):A {
      Try(a) match
        case Success(a) => a
        case Failure(ex) => throw   
   }*/ 

}

    
val Scope = FutureScope(ExecutionContext.Implicits.global)
 

class TestFutureScopeBase {

  import scala.concurrent.ExecutionContext.Implicits.global


  def foo(x:Int):Future[Int] =
    Future successful x+1

  @Test def testFutureScopeBase() = {

      var y = 0
      val f = async[Future].in(Scope) {
         val x1 = await(foo(1))
         summon[FutureScopeContext].onFinish{y = x1+1}
         x1
      }
      FutureCompleter(f.map(x => assert(x==2)))
  }


}
