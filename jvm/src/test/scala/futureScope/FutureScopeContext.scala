package futureScope

import java.util.concurrent.atomic.*
import java.util.concurrent.ConcurrentLinkedDeque
import java.util.concurrent.ConcurrentHashMap

import scala.concurrent.*
import scala.util.*

import cps.*
import cps.monads.{given,*}



/**
 * ScopedContext - bring structured concurrency primitives for futures.
 **/
class FutureScopeContext(ec: ExecutionContext, parentScope: Option[FutureScopeContext] = None) extends CpsMonadContext[Future] {

  final val INITIAL=0 
  final val CANCELLING=1
  final val CANCELLED=2

  val state = new AtomicInteger(INITIAL) 
  private val cancelWaits: ConcurrentHashMap[Promise[?],Promise[?]] = new ConcurrentHashMap() 
  private val childScopes: ConcurrentHashMap[FutureScopeContext, FutureScopeContext] = new ConcurrentHashMap()   
  private val onFinishCallbacks: AtomicReference[List[()=>Unit]] = new AtomicReference(Nil)


  override def adoptAwait[A](fa: Future[A]):Future[A] =
    given ExecutionContext = ec
    state.get match
      case INITIAL =>
        val p = Promise[A]()
        cancelWaits.put(p,p)
        fa.onComplete{ x =>
          cancelWaits.remove(p)
          x match
            case Success(x) => p.trySuccess(x)
            case Failure(x) => p.tryFailure(x)
          }
        p.future
      case CANCELLING =>
        Future failed CancelException()
      case CANCELLED =>
        Future failed CancelException()

  def cancel(): Unit = {
    if (state.compareAndSet(INITIAL,CANCELLING)) then
      val cancelException = new CancelException()
      //
      // // scalajs bug
      //cancelWaits.forEach( (k,v) =>
      //  v.nn.tryFailure(cancelException)
      //)
      val we = cancelWaits.elements().nn
      while(we.hasMoreElements) {
        val p = we.nextElement.nn
        p.tryFailure(cancelException)
      }  
      //childScopes.forEach( (k,v) =>
      //  v.nn.cancel()
      //)
      val cse = childScopes.elements().nn
      while(cse.hasMoreElements) {
        val cs = cse.nextElement.nn
        cs.cancel()
      }
      state.set(CANCELLED)
  }  
  
  def spawn[A](f: FutureScopeContext ?=> A): Future[A] = ???


  def spawnAsync[A](f: FutureScopeContext => Future[A]): Future[A] = ???

  def onFinish(callback: ()=>Unit): Unit =
    pushFinishCallback(callback)


  private def pushFinishCallback(callback: ()=>Unit): Unit = {
    var done = false
    while(!done) {
      val prev = onFinishCallbacks.get.nn
      val next = callback :: prev
      done = onFinishCallbacks.compareAndSet(prev, next)
    }
  }
  
  private def popFinishCallback(): Option[()=>Unit] = {
    var done = false
    var retval: Option[()=>Unit] = None
    while(!done) {
      val prev = onFinishCallbacks.get
      prev match
        case head::tail =>
          done = onFinishCallbacks.compareAndSet(prev, tail)
          retval = Some(head)
        case Nil =>
          done = true
    }
    retval
  }
  
  private[futureScope] def finish(): Unit =
    while{
      popFinishCallback() match
        case Some(cb) =>
          cb.apply()
          true
        case None =>
          false
    } do ()

}

