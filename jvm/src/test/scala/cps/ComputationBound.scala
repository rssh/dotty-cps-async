package cps

import scala.concurrent.Future
import scala.concurrent.duration._
import scala.quoted._
import scala.language.postfixOps
import scala.util.{Try,Success,Failure}
import scala.util.control.NonFatal
import scala.compiletime.summonFrom
import java.util.concurrent.atomic.AtomicReference
import java.util.concurrent.ConcurrentLinkedQueue
import java.util.concurrent.TimeoutException

import cps.runtime.*
import cps.testconfig.given
import cps.macros.flags.UseLoomAwait

trait ComputationBound[+T] {
 
 /**
  * run computation and return a failure with TimeoutException
  * if this computation was not fully computed during timeout.
  *
  * Note, that this function is blocks current thread, so
  * you should not use one in async environment. 
  **/    
  def run(timeout: Duration = Duration.Inf): Try[T] =
    fulfill(timeout) match 
      case Some(v) => v
      case None => Failure(new TimeoutException())

  /**
   * run computation and return Some(result) if it was 
   * fully finished during timeout or None if not
   *
   * Note, that this function is blocks current thread, so
   * you should not use one in async environment. 
   **/    
  def fulfill(timeout: Duration): Option[Try[T]] = 
      progressTimeoutBlocking(timeout) match {
            case Done(t) => Some(Success(t))
            case Error(e) => Some(Failure(e))
            case _ => None
      }

  /**
   * run computation during some time and return
   * a current progress, which can be other computation then original
   * or Done/Error when computation is completed.
   *
   * Note, that this function is blocks current thread, so
   * you should not use one in async environment. 
   **/               
  def progressTimeoutBlocking(timeout: Duration): ComputationBound[T] =
    if (timeout != Duration.Inf) {
       progress(System.currentTimeMillis + timeout.toMillis, 0, true, true)
    } else {
       var current = this
       while{
         current match
           case Done(v) => false
           case Error(ex) => false
           case _ =>
              current = progress(System.currentTimeMillis + 1000, 0, true, true)
              true
       } do ()
       current
    }



  /**
   * run computation during some time and return
   * a current progress, which can be other computation then original
   * or Done/Error when computation is completed.
   *
   * When computation is block current thread or number of nested calls 
   * is bigger than some constant, return partially progressed result
   *
   **/               
  def progress(deadline: Long, nNestedCalls: Int, runAdvanceQueue: Boolean = true, waitDeadline: Boolean = false): ComputationBound[T] 



  /**
   * needed for compability with javascript version.
   */
  def runTicks(timeout: Duration): Future[T] = 
    import scala.concurrent.ExecutionContext.Implicits.global
    Future{
      progress(timeout) match {
        case Done(t) => t
        case Error(e) => throw e
        case _ => throw new TimeoutException()
      }
    } 

  def map[S](f:T=>S): ComputationBound[S] =
     flatMap( x => Done(f(x)) ) 

  def flatMap[S](f: T=> ComputationBound[S]): ComputationBound[S] 

  def  flatMapTry[S](f: Try[T] => ComputationBound[S]): ComputationBound[S]     

}

object ComputationBound {

   import scala.collection.immutable.Queue
   
   def pure[T](value:T): ComputationBound[T] = Done(value)

   def asyncCallback[A](source: (Try[A]=>Unit)=>Unit): ComputationBound[A] = {
        val ref = new AtomicReference[Option[Try[A]]](None)
        source( r => {
          ref.set(Some(r))
          externalAsyncNotifier.synchronized{
             externalAsyncNotifier.notify()
          }
        } )
        Wait(ref, fromTry[A] _ )
   }

   def spawn[A](op: =>ComputationBound[A]):ComputationBound[A] = {
        val ref = new AtomicReference[Option[Try[A]]](None)
        val waiter = Wait[A,A](ref, fromTry[A] _ )
        val deferred = Deferred(ref, Some(Thunk( () => op ))) 
        println(s"ComputationBound::spawn, ref=$ref, deferred item=${deferred}")
        deferredQueue.add(deferred)
        waiter
   }

   def fromTry[A](t: Try[A]):ComputationBound[A] =
      t match 
        case Success(a) => Done(a)
        case Failure(e) => Error(e)
        
   def  tryOp[A](op: =>ComputationBound[A]): ComputationBound[A] =
      try {
        op
      } catch {
        case NonFatal(ex) => Error(ex)
      }

    case class Deferred[A](ref: AtomicReference[Option[Try[A]]],
                     optComputations: Option[ComputationBound[A]]) {

      type Value = A             

    }
   
    // TODO: make private back after debug
   val deferredQueue: ConcurrentLinkedQueue[Deferred[?]] = new ConcurrentLinkedQueue()
   //val traceDeferredQueue: 
   private[cps] val waitQuant = (100 millis).toMillis
   private val externalAsyncNotifier = new { }

   private[cps] final val MAX_NESTED_CALLS = 100


   def  advanceDeferredQueue(endMillis: Long, doWait: Boolean): Boolean = {
      println(s"advanceQueue: $endMillis $doWait, isEmptu = ${deferredQueue.isEmpty}")
      var nFinished = 0
      val secondQueue = new ConcurrentLinkedQueue[Deferred[?]]
      while(!deferredQueue.isEmpty && System.currentTimeMillis < endMillis) 
        val c = deferredQueue.poll()
        println(s"advanceQueue: c=$c")
        if (c != null) then
          val cr: Option[?] = c.nn.ref.get().nn
          cr match 
            case Some(r) => nFinished += 1
            case None =>
               c.optComputations match
                  case Some(r) =>     
                    r match
                       case Wait(ref1, _) if ref1.get().nn.isEmpty => 
                         println("advanceQueue: wait, added to second")
                         secondQueue.add(c)
                       case Done(v) =>
                         nFinished = nFinished + 1
                         c.ref.set(Some(Success(v)))
                       case Error(e) => nFinished = nFinished + 1
                         c.ref.set(Some(Failure(e))) 
                       case _ =>
                         var inProgress = r 
                         if (false & loomInTestconfig && Loom.isEnabled()) {
                            val nextRef = new AtomicReference[Option[Try[ComputationBound[c.Value]]]](None)
                            Loom.startVirtualThread(() => {
                              try {
                                val next = inProgress.progressNoBlock(endMillis, 0, false)
                                nextRef.set(Some(Success(next)))
                              } catch {
                                case NonFatal(ex) =>
                                  nextRef.set(Some(Failure(ex)))
                              } finally {
                                externalAsyncNotifier.synchronized {
                                  externalAsyncNotifier.notifyAll()
                                }
                              }
                            }) 
                            val nextWait = Wait(nextRef, {
                                                  case Success(cb) => cb
                                                  case Failure(ex) => Error(ex)
                                           })
                            secondQueue.add(Deferred(c.ref,Some(nextWait)))
                         } else {
                          while {
                            println(s"advanceQieie:: before progressNoBlock for $inProgress")
                            inProgress = inProgress.progress( endMillis, 0, false, false)
                            println(s"advanceQieie:: after progressNoBlock for $inProgress")
                            inProgress match
                              case Done(x) => 
                                nFinished = nFinished + 1
                                println(s"advanceQueue: done, ref=")
                                c.ref.set(Some(Success(x)))
                                false
                              case Error(e) => 
                                nFinished = nFinished + 1
                                c.ref.set(Some(Failure(e)))
                                false
                              case Thunk(f) => 
                                if (System.currentTimeMillis < endMillis) {
                                  true
                                } else {
                                  secondQueue.add(Deferred(c.ref, Some(inProgress)))
                                  false
                                }
                              case nextWait@Wait(ref2, op2) =>
                                if (ref2.get.nn.isEmpty || System.currentTimeMillis >= endMillis) {
                                  secondQueue.add(Deferred(c.ref, Some(inProgress)))
                                  false
                                } else {
                                  true
                                }
                          } do ()
                         }
                         //val nextR = r.fullfill((endNanos - System.nanoTime) nanos)
                         //nextR match
                         //  case Some(x) =>
                         //     c.ref.set(Some(x))
                         //     nFinished += 1
                         //  case None =>
                         //     secondQueue.add(Deferred(c.ref,Some(nextR)))
                  case None =>
                    // wait next time
                    secondQueue.add(c)
      while(!secondQueue.isEmpty)
         val r = secondQueue.poll()
         if r != null then
            deferredQueue.add(r.nn)
      if (nFinished == 0 && doWait)
         val timeToWait = math.min(waitQuant, endMillis - System.currentTimeMillis)
         val timeToWaitMillis = (timeToWait nanos).toMillis
         if (timeToWaitMillis > 0) 
           externalAsyncNotifier.synchronized {
              externalAsyncNotifier.wait(timeToWaitMillis)
           }
      nFinished > 0
   }

   def eagerMemoize[T](f: ComputationBound[T]): ComputationBound[T] =
        spawn(f)
      
   def lazyMemoize[T](f: ComputationBound[T]): ComputationBound[T] =
        Thunk(() => spawn(f))  
        

   transparent inline def loomInTestconfig: Boolean = 
        summonFrom {
          case given UseLoomAwait.type => true
          case _ => false
        }

   def wrapVirtualThread[A](op: =>ComputationBound[A]):ComputationBound[A] =
      if (loomInTestconfig && Loom.isEnabled) {
         val nextRef: AtomicReference[Option[ComputationBound[A]]] = new AtomicReference(None)
         Loom.startVirtualThread{ () =>
              val next = try {
                            op
                         } catch {
                            case NonFatal(ex) => Error(ex)
                         }
              nextRef.set(Some(Success(next))) 
         }
         Wait(nextRef, {
           case Success(r) => r
           case Failure(ex) => Error(ex) 
         })
      } else {
        try {
          op
        } catch {
          case NonFatal(ex) => Error(ex)
        }
      }
            

}


implicit object ComputationBoundAsyncMonad extends CpsAsyncMonad[ComputationBound] with CpsMonadInstanceContext[ComputationBound] {

   type WF[T] = ComputationBound[T]

   def pure[T](value:T): ComputationBound[T] = ComputationBound.pure(value)

   def finalAwait[T](t:ComputationBound[T]):Try[T] = t.run()

   def map[A,B](fa:ComputationBound[A])(f: A=>B): ComputationBound[B] = fa.map(f)

   def flatMap[A,B](fa:ComputationBound[A])(f: A=>ComputationBound[B]):ComputationBound[B] = 
            fa.flatMap(f) 

   def error[T](e: Throwable):ComputationBound[T] = Error[T](e)

   //override def mapTry[A,B](fa: ComputationBound[A])(f:Try[A]=>B): ComputationBound[B] =
        //  Thunk(() => Done(f(fa.run())))

   override def flatMapTry[A,B](fa: ComputationBound[A])(f:Try[A]=>ComputationBound[B]): ComputationBound[B] =
        fa.flatMapTry(f)
        //Thunk(() => f(fa.run()))

   override def restore[A](fa: ComputationBound[A])(fx:Throwable => ComputationBound[A]): ComputationBound[A] = 
         flatMapTry(fa) {
           case Success(a) => Done(a)
           case Failure(ex) => ComputationBound.wrapVirtualThread(fx(ex))
         }

   //override def withAction[A](fa:ComputationBound[A])(action: =>Unit):ComputationBound[A] = Thunk(() => {
   //      val r = fa.run()  
   //      action
   //      r match {
   //        case Success(a) => Done(a)
   //        case Failure(ex) => Error(ex)
   //      }
   //    })

   def adoptCallbackStyle[A](source: (Try[A]=>Unit) => Unit):ComputationBound[A] = 
         ComputationBound.asyncCallback(source)

   def spawn[A](op: => ComputationBound[A]): ComputationBound[A] =
          ComputationBound.spawn(op)

   def fulfill[T](t: ComputationBound[T], timeout: Duration): Option[Try[T]] =
          t.fulfill(timeout)



}

case class Thunk[T](thunk: ()=>ComputationBound[T]) extends ComputationBound[T] {
             

  def progress(deadline: Long, runAdvanceQueue: Boolean, waitDeadline: Boolean): ComputationBound[T] = {
    if (System.currentTimeMillis() >= deadline) {
      this
    } else {
      val next = if (ComputationBound.useLoomAwait && Loom.isEnabled()) {
              ComputationBound.wrapVirtualThread( thunk() )
            } else {
              Thunk.runNested(deadline,0)
            }
      next match
        case Done(t) => next
        case Error(ex) => next
        case other => other.progress(deadline, runAdvanceQueue, waitDeadline)
    }
  }

  def flatMap[S](f: T=> ComputationBound[S]): ComputationBound[S] =
     Thunk[S]{ () => thunk() match 
                 case Done(t) =>  f(t)
                 case Error(e) => Error(e)
                 case Thunk(f1) => f1().flatMap(f)
                 case Wait(ref,f1) => Wait(ref, x => f1(x).flatMap(f))
             }
   
  def  flatMapTry[S](f: Try[T] => ComputationBound[S]): ComputationBound[S] =
    Thunk[S]( () =>
      try {
        thunk() match
          case Done(t) => f(Success(t))
          case Error(e) => f(Failure(e))
          case Thunk(f1) => 
                      ComputationBound.tryOp(f1()).flatMapTry(f)
          case Wait(ref, f1) => Wait(ref, 
                x => ComputationBound.tryOp(f1(x)).flatMapTry(f))
                      
      } catch {
        case NonFatal(ex) =>
          f(Failure(ex))
      }
    )    

}

case class Done[T](value:T) extends ComputationBound[T]:

  override def fulfill(timeout: Duration): Option[Try[T]] = Some(Success(value))

  override def progressNoBlock(deadline: Long, runAdvanceQueue: Boolean, waitDeadline: Boolean): ComputationBound[T] = this

  override def flatMap[S](f: T=>ComputationBound[S] ): ComputationBound[S] =
     Thunk( () => f(value) )

  override def flatMapTry[S](f: Try[T]=>ComputationBound[S] ): ComputationBound[S] =
    Thunk( () => f(Success(value)) )
 


case class Error[T](e: Throwable) extends ComputationBound[T]:

  override def fulfill(timeout: Duration): Option[Try[T]] = Some(Failure(e))

  override def progressDeadline(deadline: Long): ComputationBound[T] = this

  override def progressNoBlock(deadline: Long, nNestedCalls: Int, runAdvanceQueue: Boolean): ComputationBound[T] = this

  override def flatMap[S](f: T=> ComputationBound[S]): ComputationBound[S] =
     this.asInstanceOf[ComputationBound[S]]

  override def flatMapTry[S](f: Try[T]=>ComputationBound[S] ): ComputationBound[S] =
      Thunk( () => f(Failure(e)) )


case class Wait[R,T](ref: AtomicReference[Option[Try[R]]], op: Try[R] => ComputationBound[T]) extends ComputationBound[T] {
       

  override def progress(deadline: Long, nNestedCalls: Int, runAdvanceQueue: Boolean, waitDeadline: Boolean): ComputationBound[T] = {
      ref.get match 
        case Some(r) => 
          val next = ComputationBound.wrapVirtualThread(op(r))
          next.progress(deadline, nNestedCalls + 1, runAdvanceQueue, waitDeadline)
        case None =>
          if (runAdvanceQueue) {
            val wasProgress = ComputationBound.advanceDeferredQueue(deadline, waitDeadline)
            if (wasProgress && nNestedCalls < ComputationBound.MAX_NESTED_CALLS) {
                progress(deadline, nNestedCalls + 1, runAdvanceQueue, waitDeadline)
            } else {
                this
            }
          } else this
  }
      
  override def flatMap[S](f: T => ComputationBound[S]): ComputationBound[S] =
        Wait(ref, x => op(x) flatMap f)

  override def flatMapTry[S](f: Try[T] => ComputationBound[S]): ComputationBound[S] =
          Wait(ref, x => ComputationBound.tryOp(op(x)) flatMapTry f)
  

  override def map[S](f: T=>S): ComputationBound[S] =
        Wait(ref, x => op(x).map(f) )


}

