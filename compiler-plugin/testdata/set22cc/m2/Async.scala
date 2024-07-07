package gears.async

import cps.*
import cps.plugin.*
import gears.async.JSAsync.JSAsyncCpsMonad


trait Async extends CpsTryMonadContext[JSAsync]:

   val support: AsyncSupport

   val scheduler: support.Scheduler

   def await[T](src: Async.Source[T])(using AsyncContext): T

   def group: CompletionGroup
   
   def withGroup(group: CompletionGroup): Async

   override val monad: CpsTryMonad[JSAsync] = new JSAsync.JSAsyncCpsMonad(this)

end Async


object Async:

   //
   inline def apply[T](inline body: Async ?=> T ): JSAsync[T] =
     cpsAsyncApply[JSAsync,T, Async](JSAsyncCpsMonad(Async.empty), body(using summon[Async]) )


   inline def current(using async: Async): Async = async

   opaque type Spawn <: Async = Async

   def group[T](body: Async.Spawn ?=> T)(using AsyncContext): T =
      withNewCompletionGroup(CompletionGroup().link())(body)

   def group_async[T](body: Async.Spawn => JSAsync[T])(using AsyncContext): T =
      withNewCompletionGroupAsync(CompletionGroup().link())(body)

   case class DefaultAsync(group: CompletionGroup = CompletionGroup.Unlinked) extends Async:
      override val support: JSAsyncSupport.type = JSAsyncSupport
      override val scheduler = support.scheduler
      override def await[T](src: Async.Source[T])(using AsyncContext): T = ???
      override def withGroup(group: CompletionGroup): Async = copy(group = group)

   private[async] def empty: Async = DefaultAsync()

   /** Runs a body within another completion group. When the body returns, the group is cancelled and its completion
    * awaited with the `Unlinked` group.
    */
   private[async] def withNewCompletionGroup[T](group: CompletionGroup)(body: Async.Spawn ?=> T)(using ac: AsyncContext): T = {
     withNewCompletionGroupAsync(group)( spawn => JSAsync.Pure(body(using spawn)) )
   }


   private[async] def withNewCompletionGroupAsync[T](group: CompletionGroup)(body: Async.Spawn => JSAsync[T])(using ac: AsyncContext): T = {
     val completionAsync =
          if CompletionGroup.Unlinked == ac.async.group
            then ac.async
          else ac.async.withGroup(CompletionGroup.Unlinked)

        try
          // compiler crash here
          await(body(summon[Async].withGroup(group)))
        finally
          group.cancel()
          given Async = completionAsync
          group.waitCompletion()
   }


  /** An asynchronous data source. Sources can be persistent or ephemeral. A persistent source will always pass same
   * data to calls of [[Source!.poll]] and [[Source!.onComplete]]. An ephemeral source can pass new data in every call.
   *
   * @see
   *   An example of a persistent source is [[gears.async.Future]].
   * @see
   *   An example of an ephemeral source is [[gears.async.Channel]].
   */
   trait Source[+T]:

     /**Checks whether data is available at present and pass it to `k` if so. Calls to `poll` are always synchronous and
      * non-blocking.
      */
     def poll(k: Listener[T]): Boolean

     /**
      * Once data is available, pass it to the listener `k`. `onComplete` is always non-blocking.
      */
     def onComplete(k: Listener[T]): Unit

     /** Signal that listener `k` is dead (i.e. will always fail to acquire locks from now on), and should be removed
       * from `onComplete` queues.
       */
     def dropListener(k: Listener[T]): Unit

     def poll(): Option[T] = ???

   end Source

   abstract class OriginalSource[+T] extends Source[T]:

      /** Add `k` to the listener set of this source. */
      protected def addListener(k: Listener[T]): Unit

      def onComplete(k: Listener[T]): Unit = synchronized:
        if !poll(k) then addListener(k)

   end OriginalSource


end Async




