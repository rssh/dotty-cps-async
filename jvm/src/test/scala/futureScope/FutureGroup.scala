package futureScope

import cps.*
import cps.monads.{*,given}
import cps.stream.{*,given}

import scala.concurrent.*

import java.util.concurrent.ConcurrentLinkedDeque

trait FutureGroup[E] extends Cancellable {

   def events: AsyncIterator[Future, E]

   def cancel(): CancellationState

   def spawn(op: FutureScopeContext ?=> Future[E]): Unit = ???
   
   def spawnAsync(op: FutureScopeContext ?=> Future[E]): Unit = ???
 
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
            val  group = new DefaultFutureGroup[E](using ctx)
            val it = value.iterator
            while(it.hasNext) {
               val c: (FutureScopeContext ?=> Future[E]) = it.next
               group.spawnAsync( (ctx) ?=> c(using ctx) )
            }
            group

   end Builder

   def collect[E](runners: Iterable[FutureScopeContext ?=> Future[E]])(using FutureScopeContext): FutureGroup[E] =
      build[Iterable[FutureScopeContext ?=> Future[E]]](runners)

   def build[A](x: A)(using builder: Builder[A], ctx: FutureScopeContext): FutureGroup[builder.Event] = {
      builder.build(ctx,x)
   }

   

}

class DefaultFutureGroup[E](using ctx: FutureScopeContext) extends FutureGroup[E] {
  
   def events: AsyncIterator[Future, E] = ???

   def cancel(): CancellationState = ???

   override def spawn(op: FutureScopeContext ?=> Future[E]): Unit = ???
   
   override def spawnAsync(op: FutureScopeContext ?=> Future[E]): Unit = ???


}
