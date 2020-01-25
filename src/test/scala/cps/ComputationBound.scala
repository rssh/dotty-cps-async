package cps

import scala.quoted._
import scala.util.{Try,Success,Failure}
import scala.concurrent.duration._
import java.util.concurrent.TimeoutException
import java.util.concurrent.atomic.AtomicReference

trait ComputationBound[T] {
 
  def run(timeout: Duration = Duration.Inf): Try[T] =
    fulfill(timeout) match 
      case Some(v) => v
      case None => Failure(new TimeoutException())

  def fulfill(timeout: Duration): Option[Try[T]] = 
        progress(timeout) match 
           case Done(t) => Some(Success(t))
           case Error(e) => Some(Failure(e))
           case _ => None

  def progress(timeout: Duration): ComputationBound[T]

  def map[S](f:T=>S): ComputationBound[S] =
     flatMap( x => Done(f(x)) )

  def flatMap[S](f: T=> ComputationBound[S]): ComputationBound[S]

}

object ComputationBound {

   import scala.collection.immutable.Queue
   
   def pure[T](value:T): ComputationBound[T] = Done(value)

   def asyncCallback[A](source: (Try[A]=>Unit)=>Unit): ComputationBound[A] = {
        val ref = new AtomicReference[Option[Try[A]]]()
        source( r => {
          ref.set(Some(r))
          externalAsyncNotifier.notify()
        } )
        Wait(ref, fromTry[A] _ )
   }

   def spawn[A](op: =>ComputationBound[A]):ComputationBound[A] = {
        val ref = new AtomicReference[Option[Try[A]]]()
        val waiter = Wait[A,A](ref, fromTry[A] _ )
        deferredQueue = deferredQueue.enqueue(Deferred(ref, Some(Thunk( () => op )) ))
        waiter
   }

   def fromTry[A](t: Try[A]):ComputationBound[A] =
      t match 
        case Success(a) => Done(a)
        case Failure(e) => Error(e)
        

   case class Deferred[A](ref: AtomicReference[Option[Try[A]]],
                     optComputations: Option[ComputationBound[A]])
   
   private val externalAsyncNotifier = new { }
   private var deferredQueue: Queue[Deferred[?]] = Queue()
   private val waitQuant = (100 millis).toNanos

   def  advanceDeferredQueue(endNanos: Long): Boolean = {
      var nFinished = 0
      var secondDeferred: Queue[Deferred[_]] = Queue()
      while(!deferredQueue.isEmpty && System.nanoTime < endNanos) 
        val (c, q) = deferredQueue.dequeue
        deferredQueue = q
        c.ref.get() match 
          case Some(r) => nFinished += 1
          case None =>
               c.optComputations match
                  case Some(r) =>     
                    val nextR = r.progress((endNanos - System.nanoTime) nanos)
                    nextR.fulfill((endNanos - System.nanoTime) nanos) match
                      case Some(x) =>
                        c.ref.set(Some(x))
                        nFinished += 1
                      case None =>
                        secondDeferred = secondDeferred.enqueue(Deferred(c.ref,Some(nextR)))
                  case None =>
                    secondDeferred = secondDeferred.enqueue(c) 
      if (deferredQueue.isEmpty)
         deferredQueue = secondDeferred
      else
         deferredQueue = deferredQueue.enqueueAll(secondDeferred)
      if (nFinished == 0)
         val timeToWait = math.min(waitQuant, endNanos - System.nanoTime)
         if (timeToWait > 0) 
           externalAsyncNotifier.synchronized {
              externalAsyncNotifier.wait((timeToWait nanos).toMillis)
           }
      nFinished > 0
   }

}

implicit object ComputationBoundAsyncMonad extends AsyncMonad[ComputationBound] {

   def pure[T](value:T): ComputationBound[T] = ComputationBound.pure(value)

   def finalAwait[T](t:ComputationBound[T]):Try[T] = t.run()

   def map[A,B](fa:ComputationBound[A])(f: A=>B):ComputationBound[B] = fa.map(f)

   def flatMap[A,B](fa:ComputationBound[A])(f: A=>ComputationBound[B]):ComputationBound[B] = 
            fa.flatMap(f) 

   def error[T](e: Throwable):ComputationBound[T] = Error[T](e)

   def restore[A](fa: ComputationBound[A])(fx:Throwable => ComputationBound[A]): ComputationBound[A] = Thunk(() => {
         fa.run() match 
            case Success(a) => Done(a)
            case Failure(ex) => fx(ex)
       })

   def withAction[A](fa:ComputationBound[A])(action: =>Unit):ComputationBound[A] = Thunk(() => {
         val r = fa.run()  
         action
         r match {
           case Success(a) => Done(a)
           case Failure(ex) => Error(ex)
         }
       })

   def adoptCallbackStyle[A](source: (Try[A]=>Unit) => Unit):ComputationBound[A] = 
         ComputationBound.asyncCallback(source)

   def spawn[A](op: =>ComputationBound[A]): ComputationBound[A] =
          ComputationBound.spawn(op)

   def fulfill[T](t: ComputationBound[T], timeout: Duration): Option[Try[T]] =
          t.fulfill(timeout)


}

case class Thunk[T](thunk: ()=>ComputationBound[T]) extends ComputationBound[T] {


  def progress(timeout: Duration): ComputationBound[T] = 
        thunk() match 
           case r@Done(t) => r
           case r@Error(e) => r
           case w@Wait(ref, f) => w.progress(timeout)
           case Thunk(f1) => f1().progress(timeout)


  def flatMap[S](f: T=> ComputationBound[S]): ComputationBound[S] =
     Thunk[S]{ () => thunk() match 
                 case Done(t) =>  f(t)
                 case Error(e) => Error(e)
                 case Thunk(f1) => f1().flatMap(f)
                 case Wait(ref,f1) => Wait(ref, x => f1(x).flatMap(f))
             }
     
}

case class Done[T](value:T) extends ComputationBound[T] 

  override def fulfill(timeout: Duration): Option[Try[T]] = Some(Success(value))

  override def progress(timeout: Duration): ComputationBound[T] = this

  override def flatMap[S](f: T=>ComputationBound[S]): ComputationBound[S] =
     Thunk( () => f(value) )


case class Error[T](e: Throwable) extends ComputationBound[T] 

  override def fulfill(timeout: Duration): Option[Try[T]] = Some(Failure(e))

  override def progress(timeout: Duration): ComputationBound[T] = this

  override def flatMap[S](f: T=> ComputationBound[S]): ComputationBound[S] =
     this.asInstanceOf[ComputationBound[S]]


case class Wait[R,T](ref: AtomicReference[Option[Try[R]]], op: Try[R] => ComputationBound[T]) extends ComputationBound[T] {

  def progress(timeout: Duration): ComputationBound[T] = 
     ref.get match
       case Some(r) => op(r).progress(timeout)
       case None =>
         val beforeWait = Duration(System.nanoTime, NANOSECONDS)
         val endTime = (beforeWait + timeout).toNanos
         if (timeout.isFinite) 
            while(ref.get().isEmpty && ( System.nanoTime < endTime ) )
               ComputationBound.advanceDeferredQueue(endTime)
         else
            while(ref.get().isEmpty)
               ComputationBound.advanceDeferredQueue(endTime)
         ref.get.map{ r => 
             val afterWait = Duration(System.nanoTime, NANOSECONDS)
             op(r).progress(timeout - (afterWait - beforeWait)) 
         }.getOrElse(this)
      
  override def flatMap[S](f: T => ComputationBound[S]): ComputationBound[S] =
        Wait(ref, x => op(x) flatMap f)

  override def map[S](f: T=>S): ComputationBound[S] =
        Wait(ref, x => op(x).map(f) )

}

