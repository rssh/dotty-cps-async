package futureScope

import java.util.concurrent.atomic.*
import java.util.concurrent.ConcurrentLinkedDeque
import java.util.concurrent.ConcurrentHashMap

import scala.collection.mutable.ArrayBuffer
import scala.concurrent.*
import scala.util.*
import scala.util.control.*

import cps.*
import cps.monads.{given,*}



/**
 * ScopedContext - bring structured concurrency primitives for futures.
 **/
class FutureScopeContext(ec: ExecutionContext, parentScope: Option[FutureScopeContext] = None) extends CpsMonadContext[Future] with Cancellable {

  val stateRef = new AtomicReference[FutureScopeContext.State](FutureScopeContext.State.Active) 
  private val cancellables: ConcurrentHashMap[Cancellable,Cancellable] = new ConcurrentHashMap() 
  //TODO: injext
  //private val contCallbacks: ConcurrentLinkedDeque[(FutureScopeContext) ?=> Future[Unit]]
  private val finishCallbacks: ConcurrentLinkedDeque[() => Future[Unit]] = new ConcurrentLinkedDeque()


  def executionContext: ExecutionContext = ec

  override def adoptAwait[A](fa: Future[A]):Future[A] =
    given ExecutionContext = ec
    stateRef.get match
      case FutureScopeContext.State.Active =>
        val p = Promise[A]()
        val c = CancellablePromise(p)
        fa.onComplete{ x =>
          cancellables.remove(c)
          x match
            case Success(x) => p.trySuccess(x)
            case Failure(x) => p.tryFailure(x)
          }  
        p.future
      case _ =>
        Future failed ScopeCancellationException()

  def cancel(ex: ScopeCancellationException): CancellationResult = {

    given ExecutionContext = ec

    def collectFinishing(): Seq[Future[CancellationResult]] = {
      val cit = cancellables.elements().nn
      val finishing: ArrayBuffer[Future[CancellationResult]] = new ArrayBuffer()
      while(cit.hasMoreElements) {
        val c = cit.nextElement.nn
        c.cancel(ex) match
          case CancellationResult.Cancelling(cFinishing) =>
             finishing.addAll(cFinishing)
          case _ =>
      }
      finishing.toSeq
    }

    if (stateRef.compareAndSet(FutureScopeContext.State.Active,FutureScopeContext.State.Cancelling(ex))) then
      //
      // // scalajs bug
      //cancelWaits.forEach( (k,v) =>
      //  v.nn.tryFailure(cancelException)
      //)
      val finishing = collectFinishing()
      if (finishing.isEmpty) then
        stateRef.set(FutureScopeContext.State.Finished)
        CancellationResult.Cancelled
      else
        Future.sequence(finishing).onComplete{ _ =>
          stateRef.set(FutureScopeContext.State.Finished)
        }
        // TODO: add our destructors.
        CancellationResult.Cancelling(finishing)
    else if (stateRef.get().nn.isInstanceOf[CancellationResult.Cancelling]) then  
      val finishing = collectFinishing()
      if (finishing.isEmpty) then
        CancellationResult.AlreadyFinished
      else
        CancellationResult.Cancelling(finishing)  
    else 
      CancellationResult.AlreadyFinished
  }  


  
  def spawn[A](f: FutureScopeContext ?=> A, executionContext: ExecutionContext = ec): Future[A] = 
    spawnAsync( ctx ?=>  Future successful f(using ctx), executionContext)


  def spawnAsync[A](f: FutureScopeContext ?=> Future[A], executionContext: ExecutionContext = ec): Future[A] = {
    given ExecutionContext = executionContext
    stateRef.get() match
      case FutureScopeContext.State.Active =>
        val c = new FutureScopeContext(executionContext, Some(this))
        val p = Promise[A]()
        // TODO: now it.s run in 
        p.future.onComplete{ _ =>
           // finish called async.
           c.finish()
        }
        addCancellable(FutureScopeContext.ChildRecord(c,p))
        val r = new Runnable() {
           override def run(): Unit = {
              try
                p.completeWith(f(using c))
              catch
                case NonFatal(ex) =>
                  p.tryFailure(ex)
           }
        }
        executionContext.execute(r)
        p.future
      case _ =>
        throw new IllegalStateException("f is already cancelling")
  }



  def addCancellable(c: Cancellable): Unit = {
      cancellables.put(c,c)
  } 

  def onFinish(callback: => Unit): Unit =
      onFinishAsync{
        () => Future successful callback
      }

  def onFinishAsync(callback: () => Future[Unit]): Unit =
      finishCallbacks.addFirst(callback)
  
  private[futureScope] def finish(): Future[Unit] =
    given ExecutionContext = ec
    async[Future] {
      // TODO:  add timeout.
      // TODO: run finish before cancellation
      try 
        await(runCancellation(ScopeFinished()))
      finally  
        await(runFinishCallbacks())
    }

  private def runCancellation(ex: ScopeCancellationException): Future[Unit] =
       // TODO:  add timeout exception during some deadline 
      waitCancellation(cancel(ex))   

  private def waitCancellation(cancellationResult: CancellationResult): Future[Unit] =
    given ExecutionContext = ec
    async[Future] {
      cancellationResult match
        case CancellationResult.Cancelling(finishing) =>
            for( fc <- finishing ) {
              val c = await(fc)
              await(waitCancellation(c))
            }
        case _ =>
    }    

  private[futureScope] def runFinishCallbacks(): Future[Unit] = 
    given ExecutionContext = ec
    async[Future] {
      while {
        val cb = finishCallbacks.pollFirst()
        if (cb != null) then
          // ? - what behavour we want when callback throws an exception
          // are finish callbacks should run
          await(cb.nn())
          true
        else
          false
      } do ()
    }
   
}

object FutureScopeContext {

   object StateFlags: 
     final val Active = 1
     final val Cancelling = 2
     final val Finished = 4

   enum State(val flags: Int):
      case Active extends State(StateFlags.Active)
      case Cancelling(e: ScopeCancellationException) extends State(StateFlags.Active | StateFlags.Cancelling)
      case Cancelled(e: ScopeCancellationException) extends State(StateFlags.Finished | StateFlags.Cancelling)
      case Finished extends State(StateFlags.Finished)  

      def isActive = (flags & StateFlags.Active) != 0

   end State 

   case class ChildRecord[A](ctx: FutureScopeContext, promise: Promise[A]) extends Cancellable:
      def cancel(ex: ScopeCancellationException): CancellationResult =
        promise.tryFailure(ex)
        ctx.cancel(ex) 

}

