package gears.async

import java.util.concurrent.atomic.AtomicBoolean
import scala.annotation.unchecked.uncheckedVariance
import scala.collection.mutable
import scala.compiletime.uninitialized
import scala.util.Try

trait Future[+T] extends Async.OriginalSource[Try[T]], Cancellable

object Future:

  private class CoreFuture[+T] extends Future[T]:

    @volatile protected var hasCompleted: Boolean = false
    protected var cancelRequest = AtomicBoolean(false)
    private var result: Try[T] = uninitialized // guaranteed to be set if hasCompleted = true
    private val waiting: mutable.Set[Listener[Try[T]]] = mutable.Set()

    // Async.Source method implementations

    def poll(k: Listener[Try[T]]): Boolean =
      if hasCompleted then
        k.completeNow(result, this)
        true
      else false

    def addListener(k: Listener[Try[T]]): Unit = synchronized:
      waiting += k

    def dropListener(k: Listener[Try[T]]): Unit = synchronized:
      waiting -= k

    // Cancellable method implementations

    def cancel(): Unit =
      setCancelled()

    override def link(group: CompletionGroup): this.type =
      // though hasCompleted is accessible without "synchronized",
      // we want it not to be run while the future was trying to complete.
      synchronized:
        if !hasCompleted || group == CompletionGroup.Unlinked then super.link(group)
        else this

    /** Sets the cancellation state and returns `true` if the future has not been completed and cancelled before. */
    protected final def setCancelled(): Boolean =
      !hasCompleted && cancelRequest.compareAndSet(false, true)

    /** Complete future with result. If future was cancelled in the meantime, return a CancellationException failure
     * instead. Note: @uncheckedVariance is safe here since `complete` is called from only two places:
     *   - from the initializer of RunnableFuture, where we are sure that `T` is exactly the type with which the future
     *     was created, and
     *   - from Promise.complete, where we are sure the type `T` is exactly the type with which the future was created
     *     since `Promise` is invariant.
     */
    private[Future] def complete(result: Try[T] @uncheckedVariance): Unit =
      val toNotify = synchronized:
        if hasCompleted then Nil
        else
          this.result = result
          hasCompleted = true
          val ws = waiting.toList
          waiting.clear()
          unlink()
          ws
      for listener <- toNotify do listener.completeNow(result, this)

  end CoreFuture

  private class RunnableFuture[+T](body: Async.Spawn ?=> T)(using ac: Async) extends CoreFuture[T]:



  end RunnableFuture


end Future