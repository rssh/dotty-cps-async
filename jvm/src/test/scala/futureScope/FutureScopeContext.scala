package futureScope

import java.util.concurrent.atomic.*
import java.util.concurrent.ConcurrentLinkedDeque
import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.TimeoutException

import scala.collection.mutable.ArrayBuffer
import scala.concurrent.*
import scala.concurrent.duration.*
import scala.util.*
import scala.util.control.*

import cps.*
import cps.monads.{given,*}

import futureScope.util.*

import cps.testconfig.given

/**
 * ScopedContext - bring structured concurrency primitives for futures.
 **/
class FutureScopeContext(ec: ExecutionContext, parentScope: Option[FutureScopeContext] = None) extends CpsMonadContext[Future] 
                                                                                                  with ExecutionContextProvider  
                                                                                                  with Cancellable {

  val stateRef = new AtomicReference[FutureScopeContext.State](FutureScopeContext.State.Active) 
  private val cancellableFutures: ConcurrentHashMap[CancellableFuture[?],CancellableFuture[?]] = new ConcurrentHashMap() 
  private val nonCancellables: ConcurrentHashMap[Promise[Unit],Promise[Unit]] = new ConcurrentHashMap()
  //private val blocking:
  //TODO: injext
  //private val contCallbacks: ConcurrentLinkedDeque[(FutureScopeContext) ?=> Future[Unit]]
  private val finishCallbacks: ConcurrentLinkedDeque[() => Future[Unit]] = new ConcurrentLinkedDeque()
  private val finishPromise: Promise[Unit] = Promise()
  
  def executionContext: ExecutionContext = ec

  override def adoptAwait[A](fa: Future[A]):Future[A] = {
    given ExecutionContext = ec
    stateRef.get match
      case FutureScopeContext.State.Active =>    
        if (fa eq finishPromise.future) then
          fa
        else
          val p = Promise[A]()
          val c = DelegatedCancellableFuture(p.future, 
                   ex => {
                    if p.tryFailure(ex) then
                      CancellationResult.Cancelled
                    else
                      CancellationResult.AlreadyFinished
                  }
                )
          addCancellableFuture(c)
          fa.onComplete{ x =>
            cancellableFutures.remove(c)
            x match
              case Success(x) => p.trySuccess(x)
              case Failure(x) => p.tryFailure(x)
            }  
          // stateRef can be changed after addition
          stateRef.get match
            case FutureScopeContext.State.Active =>  
                p.future
            case FutureScopeContext.State.Cancelling(e) =>    
                Future failed e      
            case FutureScopeContext.State.Cancelled(e) =>    
                Future failed e      
            case _ =>
                Future failed ScopeCancellationException()
      case FutureScopeContext.State.Cancelling(e) =>
        Future failed e
      case FutureScopeContext.State.Cancelled(e) =>
        Future failed e
      case _ =>
        // scope is finished. 
        Future failed ScopeCancellationException("scope finished")
  }


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
      val cit = cancellableFutures.elements().nn
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
        stateRef.set(FutureScopeContext.State.Cancelled(ex))
        CancellationResult.Cancelled
      else 
        finish.onComplete{ _ =>
          stateRef.set(FutureScopeContext.State.Cancelled(ex))
        }    
        CancellationResult.Cancelling(finish)
    else if (stateRef.get().nn.isInstanceOf[FutureScopeContext.State.Cancelling]) then  
      if (finishPromise.future.isCompleted) then
        CancellationResult.AlreadyFinished
      else
        CancellationResult.Cancelling(finishPromise.future)  
    else 
      CancellationResult.AlreadyFinished
  }  


  
  def spawn[A](f: FutureScopeContext ?=> A, executionContext: ExecutionContext = ec): CancellableFuture[A] = 
    spawn_async( ctx =>  Future successful f(using ctx), executionContext)
    
    
  def spawnAsync[A](f: FutureScopeContext ?=> Future[A], executionContext: ExecutionContext = ec): CancellableFuture[A] = {
    spawn_async((ctx) => f(using ctx), executionContext )
  }

  def spawn_async[A](f: FutureScopeContext => Future[A], executionContext: ExecutionContext = ec): CancellableFuture[A] = {
    given ExecutionContext = executionContext
    stateRef.get() match
      case FutureScopeContext.State.Active =>
        val c = new FutureScopeContext(executionContext, Some(this))
        val p = Promise[A]()
        p.completeWith(c.run(f))
        val childRecord = DelegatedCancellableFuture(p.future,
           (ex) => {
               p.tryFailure(ex) 
               c.cancel(ex) 
           }
        )
        addCancellableFuture(childRecord)
        childRecord
      case FutureScopeContext.State.Cancelling(e) =>
        // TODO: think, what better throw -- cancellable excwption or illegal-state ? 
        //throw new IllegalStateException("f is already cancelling", e)
        throw e
      case FutureScopeContext.State.Cancelled(e) =>  
        //throw new IllegalStateException("f is already cancelled", e)
        throw e
      case FutureScopeContext.State.Finished =>  
        throw new IllegalStateException("f is already finished")
  }

  def spawnDelay(duration: FiniteDuration): CancellableFuture[FiniteDuration] = {
    val retval = TimeOperations.waiting(duration)(duration)
    addCancellableFuture(retval)
    retval
  }

  def spawnTimeout(duration: FiniteDuration): CancellableFuture[Nothing] = {
    val retval = TimeOperations.waiting(duration){
      throw TimeoutException()
    }
    addCancellableFuture(retval)
    retval
  }



  /**
   * join will be finished after finish of all childs of this context
   **/
  transparent inline def join(): Unit = {
    // we assume here, that finishPromise.future is stable
    given ExecutionContext = ec
    given FutureScopeContext = this
    await(finishPromise.future) 
  }


  def timedAwaitAsync[A](fa: Future[A], timeout: FiniteDuration): Future[A] = {
    val p = Promise[A]()
    val retval = TimeOperations.waiting(timeout){
        p.tryFailure(TimeoutException())
    }
    addCancellableFuture(retval)
    p.completeWith(fa)
    p.future
  }

  def timedAwaitCompletedAsync[A](fa: Future[A], timeout: FiniteDuration): Future[Boolean] = {
    given ExecutionContext = executionContext
    val p = Promise[Boolean]
    val retval = TimeOperations.waiting(timeout){
        p.trySuccess(false)
    }
    addCancellableFuture(retval)
    fa.onComplete(_ => p.trySuccess(true))
    p.future
  }

  def isFinished: Boolean = {
    stateRef.get().nn.isFinished
  }

  def isActive: Boolean = {
    stateRef.get().nn.isActive
  }

  private def addCancellableFuture[A](c: CancellableFuture[A]): Unit = {
      cancellableFutures.put(c,c)
  } 


  def onFinish(callback: => Unit): Unit =
      onFinishAsync{
        () => Future successful callback
      }

  def onFinishAsync(callback: () => Future[Unit]): Unit =
      finishCallbacks.addFirst(callback)
  
  private[futureScope] def run[A](f: FutureScopeContext => Future[A]): Future[A] = {
      given ExecutionContext = ec

      def handleEscalation(ex:Throwable): Throwable = {
            ex match
              case NoEscalateExceptionWrapper(wrapped) =>
                wrapped
              case uhex:UnhandledExceptionInChildScope =>
                parentScope.foreach(_.cancel(uhex))
                ex
              case scex: ScopeCancellationException =>
                // don't touch parent scope for other cancellation exceptions
                ex  
              case _ =>
                parentScope.foreach{ scope =>
                  val uhex = UnhandledExceptionInChildScope(ex, this)
                  val r = scope.cancel(uhex)
                }
                ex
      }
        
      def afterFinalizer(v:Try[A],finish:Future[Unit]): Future[A] = {
        v match
          case Success(r) => finish.map(_ => r)
          case Failure(ex) =>
            val nex = handleEscalation(ex)
            finish.transform{
              case Success(_) => Failure(nex)
              case Failure(finEx) => nex.addSuppressed(finEx)
                                     Failure(nex)
            }
      }

      val fThis = try{
        f(this)
      }catch{
        case NonFatal(ex) => 
          Future failed ex
      }
      fThis.transformWith{ v =>
          stateRef.get match
              case FutureScopeContext.State.Active =>
                  cancel(ScopeFinished()) match
                    case CancellationResult.Cancelling(finishing) =>
                      afterFinalizer(v, finishing)
                    case _ =>
                      v match
                        case Success(r) => Future successful r
                        case Failure(ex) =>
                          val nex = handleEscalation(ex)
                          Future failed nex
              case _ =>
                   afterFinalizer(v, finishPromise.future)
      }
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
      def isFinished = (flags & StateFlags.Finished) != 0

   end State 


}

