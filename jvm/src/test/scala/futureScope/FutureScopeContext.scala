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
  private val nonCancellables: ConcurrentHashMap[Promise[Unit],Promise[Unit]] = new ConcurrentHashMap()
  //private val blocking:
  //TODO: injext
  //private val contCallbacks: ConcurrentLinkedDeque[(FutureScopeContext) ?=> Future[Unit]]
  private val finishCallbacks: ConcurrentLinkedDeque[() => Future[Unit]] = new ConcurrentLinkedDeque()
  private val finishPromise: Promise[Unit] = Promise()
  

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

    def nonCancellableWaits(): Future[Unit] =
      val it = nonCancellables.elements().nn
      var r = Future successful ()
      while(it.hasMoreElements) {
        val c = it.nextElement.nn
        r = r.transformWith(_ => c.future)
      }
      r

    def childWaits(): Future[Unit] = 
      val cit = cancellables.elements().nn
      var r = Future successful ()
      while(cit.hasMoreElements) {
        val c = cit.nextElement.nn
        c.cancel(ex) match
          case CancellationResult.Cancelling(cFinishing) =>
             r = r.transformWith(_ => cFinishing)
          case _ =>
      }
      r
    

    if (stateRef.compareAndSet(FutureScopeContext.State.Active,FutureScopeContext.State.Cancelling(ex))) then
      val finish = nonCancellableWaits().transformWith(_ =>
                      childWaits().transformWith( _ =>
                         finishPromise.completeWith(runFinishCallbacks()).future
                      )  
                   )
      if (finish.isCompleted) then 
        stateRef.set(FutureScopeContext.State.Finished)
        CancellationResult.Cancelled
      else 
        finish.onComplete{ _ =>
          stateRef.set(FutureScopeContext.State.Finished)
        }    
        CancellationResult.Cancelling(finish)
    else if (stateRef.get().nn.isInstanceOf[CancellationResult.Cancelling]) then  
      if (finishPromise.future.isCompleted) then
        CancellationResult.AlreadyFinished
      else
        CancellationResult.Cancelling(finishPromise.future)  
    else 
      CancellationResult.AlreadyFinished
  }  


  
  def spawn[A](f: FutureScopeContext ?=> A, executionContext: ExecutionContext = ec): Future[A] = 
    spawnAsync( ctx =>  Future successful f(using ctx), executionContext)


  def spawnAsync[A](f: FutureScopeContext => Future[A], executionContext: ExecutionContext = ec): Future[A] = {
    given ExecutionContext = executionContext
    stateRef.get() match
      case FutureScopeContext.State.Active =>
        val c = new FutureScopeContext(executionContext, Some(this))
        val p = Promise[A]()
        p.completeWith(c.run(f))
        addCancellable(FutureScopeContext.ChildRecord(c,p))
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
  
  private[futureScope] def run[A](f: FutureScopeContext => Future[A]): Future[A] = {
      given ExecutionContext = ec
      f(this).transformWith{ v =>
          stateRef.get match
              case FutureScopeContext.State.Active =>
                   cancel(ScopeFinished()) match
                    case CancellationResult.Cancelling(finishing) =>
                        transformSuppressed(v,finishing)
                    case _ =>
                        Future.fromTry(v)    
              case _ =>
                   transformSuppressed(v,finishPromise.future)
      }
  }

  //TODO: move to util
  private def transformSuppressed[A](ta: Try[A], fu: Future[Unit])(using ExecutionContext): Future[A] =
    fu.transform(tu => attachSuppressed(ta,tu))
 
  private def attachSuppressed[A](ta: Try[A], tu:Try[Unit]): Try[A] =
    tu match
      case Success(_) => ta
      case r@Failure(eu) =>
        ta match
          case Success(_) => Failure(eu)
          case r@Failure(ea) => ea.addSuppressed(eu)
                                r 
 

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

