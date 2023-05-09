package futureScope

import cps.*
import cps.monads.{*,given}
import cps.stream.{*,given}
import cps.plugin.annotation.CpsNotChange
import cps.testconfig.given

import scala.concurrent.*
import scala.util.*

import java.util.concurrent.ConcurrentLinkedDeque

trait FutureGroup[E] extends Cancellable  {

   def eventFlow: EventFlow[E] 

   def events: AsyncIterator[Future, E] =
      eventFlow.events

   override def cancel(ex: ScopeCancellationException): CancellationResult

   def spawn(op: FutureScopeContext ?=> E): Unit
   
   def spawnAsync(op: FutureScopeContext ?=> Future[E]): Unit
 
   def spawn_async(op: FutureScopeContext => Future[E]): Unit

}

object FutureGroup {

   trait Builder[T] {
      type Event
      def build(ctx:FutureScopeContext, value:T): FutureGroup[Event]
   } 

   object Builder:
      type Aux[T,E] = Builder[T] { type Event = E }

      given iterableBuilder[E]: Builder[Iterable[FutureScopeContext ?=> Future[E]]] with
        type Event = E

        def build(ctx: FutureScopeContext, value: Iterable[FutureScopeContext ?=> Future[E]]): FutureGroup[E] =
            val  group = new DefaultFutureGroup[E](ctx)
            val it = value.iterator
            while(it.hasNext) {
               val c: (FutureScopeContext ?=> Future[E]) = it.next
               group.spawnAsync( c )
            }
            group

   end Builder

   @cps.plugin.annotation.CpsNotChange()
   def collect[E](runners: Iterable[FutureScopeContext ?=> Future[E]])(using FutureScopeContext): FutureGroup[E] =
      build[Iterable[FutureScopeContext ?=> Future[E]]](runners)

   def build[A](x: A)(using builder: Builder[A], ctx: FutureScopeContext): FutureGroup[builder.Event] = {
      builder.build(ctx,x)
   }

   

}

class DefaultFutureGroup[E](parent: FutureScopeContext) extends FutureGroup[E] {     

   override val eventFlow = EventFlow()(using parent.executionContext) 

   val scopeContext = new FutureScopeContext(parent.monad, parent.executionContext, Some(parent))
  
   override def cancel(ex: ScopeCancellationException): CancellationResult = 
      scopeContext.cancel(ex)

   override def spawn(op: FutureScopeContext ?=> E): Unit = 
      scopeContext.spawn{ ctx ?=> 
         eventFlow.postTry(Try(op))
      }
   
   override def spawnAsync(op: FutureScopeContext ?=> Future[E]): Unit = 
      spawn_async( ctx => op(using ctx) )

   override def spawn_async(op: FutureScopeContext => Future[E]): Unit = 
      given ExecutionContext = scopeContext.executionContext
      scopeContext.spawn_async{ ctx =>
         op(ctx).transform( r => Success(eventFlow.postTry(r)))
      }
   

}
