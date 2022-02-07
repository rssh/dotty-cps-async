package cps

import scala.concurrent._
import scala.concurrent.duration._
import scala.quoted._
import scala.language.postfixOps
import scala.util.{Try,Success,Failure}
import scala.util.control.NonFatal
import java.util.concurrent.atomic.AtomicReference
import java.util.concurrent.ConcurrentLinkedQueue
import java.util.concurrent.TimeoutException
import scalajs.concurrent.JSExecutionContext.Implicits.queue


trait ComputationBound[+T] {
 
  // TODO: remove
  def run(timeout: Duration = Duration.Inf): Try[T] =
    progressImmediate(timeout) match 
      case Done(t) => Success(t)
      case Error(e) => Failure(e)
      case _ => Failure(new TimeoutException())
    

  def progress(timeout: Duration): ComputationBound[T]

  def progressImmediate(timeout: Duration): ComputationBound[T] =
    // TODO: change
    progress(timeout)


  def checkProgress(timeout: Duration = Duration.Inf): Either[Try[T],ComputationBound[T]] =
        progress(timeout) match
           case Done(t) => Left(Success(t))
           case Error(e) => Left(Failure(e))
           case other => Right(other)

  def runTicks(timeout: Duration = Duration.Inf): Future[T] =
     ComputationBound.progressWithNextTicks(this, timeout)
                            
            
  def map[S](f:T=>S): ComputationBound[S] =
     flatMap( x => Done(f(x)) )

  def flatMap[S](f: T=> ComputationBound[S]): ComputationBound[S]

  def mapTry[S](f:Try[T]=>S): ComputationBound[S] =
     flatMapTry(x => Done(f(x)))

  def flatMapTry[S](f: Try[T] => ComputationBound[S]): ComputationBound[S]

}

object ComputationBound {

   import scala.collection.immutable.Queue
   
   def pure[T](value:T): ComputationBound[T] = Done(value)

   def asyncCallback[A](source: (Try[A]=>Unit)=>Unit): ComputationBound[A] = {
        val ref = new AtomicReference[Option[Try[A]]](None)
        source( r => {
          ref.set(Some(r))
          externalAsyncNotifier.doNotifyAll()
        } )
        Wait(ref, fromTry[A] _ )
   }

   def spawn[A](op: =>ComputationBound[A]):ComputationBound[A] = {
        val ref = new AtomicReference[Option[Try[A]]](None)
        val waiter = Wait[A,A](ref, fromTry[A] _ )
        deferredQueue.add(Deferred(ref, Some(Thunk( () => op )) ))
        waiter
   }

   def fromTry[A](t: Try[A]):ComputationBound[A] =
      t match 
        case Success(a) => Done(a)
        case Failure(e) => Error(e)
        
   def eagerMemoize[T](f: ComputationBound[T]): ComputationBound[T] =
        spawn(f)


   case class Deferred[A](ref: AtomicReference[Option[Try[A]]],
                     optComputations: Option[ComputationBound[A]])
   
    // TODO: make private back after debug
   val deferredQueue: ConcurrentLinkedQueue[Deferred[?]] = new ConcurrentLinkedQueue()
   private val waitQuant = (100 millis)

   private val externalAsyncNotifier = new AsyncNotifier()

   def  advanceDeferredQueue(endNanos: Long): Boolean = {
      var nFinished = 0
      val secondQueue = new ConcurrentLinkedQueue[Deferred[?]]
      while(!deferredQueue.isEmpty && System.nanoTime < endNanos) 
        val c = deferredQueue.poll()
        if (!(c eq null)) then
          c.ref.get() match 
            case Some(r) => nFinished += 1
            case None =>
               c.optComputations match
                  case Some(r) =>     
                    r match
                       case Wait(ref1, _) if ref1.get().isEmpty  => // do nothing
                         secondQueue.add(c)
                       case _ =>
                         val nextR = r.progress((endNanos - System.nanoTime) nanos)
                         nextR match
                           case Done(x) =>
                              c.ref.set(Some(Success(x)))
                              nFinished += 1
                           case Error(x) =>
                              c.ref.set(Some(Failure(x)))
                              nFinished += 1
                           case _ =>
                              secondQueue.add(Deferred(c.ref,Some(nextR)))
                  case None =>
                    // wait next time
                    secondQueue.add(c)
      while(!secondQueue.isEmpty)
         val r = secondQueue.poll()
         if !(r eq null) then
            deferredQueue.add(r)
      nFinished > 0
   }

   def progressWithNextTicks[T](cb: ComputationBound[T], timeout: Duration): Future[T] =
        val endMillis = if (timeout.isFinite) then {
                           System.currentTimeMillis + timeout.toMillis 
                        } else {
                           System.currentTimeMillis + waitQuant.toMillis
                        }
        cb.progress(timeout) match
           case Done(t) =>  Future successful t
           case Error(e) => Future failed e
           case Thunk(r) => // impossible, because thuk is handled in progress, but
                            //  elt handle one anyway
                            progressWithNextTicks(r(),timeout)
           case w@Wait(ref, cont) =>
                 ref.get() match
                   case Some(c) =>
                      progressWithNextTicks(cont(c),timeout)
                   case None =>
                      if (timeout.isFinite) then
                        val millisToEnd = endMillis - System.currentTimeMillis
                        if (millisToEnd < 0) then
                          Future failed new TimeoutException()
                        else
                          externalAsyncNotifier.timedWait(millisToEnd millis).flatMap{ u =>
                              val millisToEnd = endMillis - System.currentTimeMillis
                              if (millisToEnd < 0) then
                                 Future failed new TimeoutException()
                              else
                                 progressWithNextTicks(w, millisToEnd.millis)
                          }
                      else
                        externalAsyncNotifier.timedWait(timeout).flatMap{ _ =>
                          progressWithNextTicks(w, timeout)
                        }
                            
                
  

}

implicit object ComputationBoundAsyncMonad extends CpsAsyncMonad[ComputationBound] with CpsMonadInstanceContext[ComputationBound]  {

   type WF[T] = ComputationBound[T]

   def pure[T](value:T): ComputationBound[T] = ComputationBound.pure(value)

   def map[A,B](fa:ComputationBound[A])(f: A=>B): ComputationBound[B] = fa.map(f)

   def flatMap[A,B](fa:ComputationBound[A])(f: A=>ComputationBound[B]):ComputationBound[B] = 
            fa.flatMap(f) 

   def error[T](e: Throwable):ComputationBound[T] = Error[T](e)

   override def mapTry[A,B](fa: ComputationBound[A])(f:Try[A]=>B): ComputationBound[B] =
     fa.mapTry(f)

   override def flatMapTry[A,B](fa: ComputationBound[A])(f:Try[A]=>ComputationBound[B]): ComputationBound[B] =
     fa.flatMapTry(f)


   def adoptCallbackStyle[A](source: (Try[A]=>Unit) => Unit):ComputationBound[A] = 
         ComputationBound.asyncCallback(source)

   def spawn[A](op: => ComputationBound[A]): ComputationBound[A] =
          ComputationBound.spawn(op)

                  
}

case class Thunk[T](thunk: ()=>ComputationBound[T]) extends ComputationBound[T] {


  def progress(timeout: Duration): ComputationBound[T] = 
         val r = try {
                   thunk() 
                 } catch {
                   case NonFatal(e) => Error(e)
                 }
         r match 
           case Done(t) => r
           case Error(e) => r
           case w@Wait(ref, f) => w.progress(timeout)
           case Thunk(f1) => 
                   // TODO:  Trampoline
                   try {
                     f1().progress(timeout)
                   } catch {
                     case NonFatal(e) => Error(e)
                   }


  def flatMap[S](f: T=> ComputationBound[S]): ComputationBound[S] =
     Thunk[S]{ () => thunk() match 
                 case Done(t) =>  f(t)
                 case Error(e) => Error(e)
                 case Thunk(f1) => f1().flatMap(f)
                 case Wait(ref,f1) => Wait(ref, x => f1(x).flatMap(f))
             }

  def flatMapTry[S](f: Try[T]=>ComputationBound[S]): ComputationBound[S] =
    Thunk[S]{ () =>
       thunk() match
        case Done(t) => f(Success(t))
        case Error(e) => f(Failure(e))
        case Thunk(f1) => f1().flatMapTry(f)
        case Wait(ref, f1) => Wait(ref, x => f1(x).flatMapTry(f))
    }
     
}

case class Done[T](value:T) extends ComputationBound[T]:

  override def progress(timeout: Duration): ComputationBound[T] = this

  override def flatMap[S](f: T=>ComputationBound[S] ): ComputationBound[S] =
     Thunk( () => f(value) )

  override def flatMapTry[S](f: Try[T]=>ComputationBound[S] ): ComputationBound[S] =
      Thunk( () => f(Success(value)) )
 


case class Error[T](e: Throwable) extends ComputationBound[T]:

  override def progress(timeout: Duration): ComputationBound[T] = this

  override def flatMap[S](f: T=> ComputationBound[S]): ComputationBound[S] =
     this.asInstanceOf[ComputationBound[S]]

  override def flatMapTry[S](f: Try[T] => ComputationBound[S] ): ComputationBound[S] =
     try {
       f(Failure(e))
     }catch{
       case ex: Throwable =>
        Error(ex)
     }


case class Wait[R,T](ref: AtomicReference[Option[Try[R]]], op: Try[R] => ComputationBound[T]) extends ComputationBound[T] {

  def progress(timeout: Duration): ComputationBound[T] = 
     ref.get match
       case Some(r) => op(r).progress(timeout)
       case None =>
         val beforeWait = Duration(System.nanoTime, NANOSECONDS)
         if (timeout.isFinite) 
            val endTime = (beforeWait + timeout).toNanos
            while(ref.get().isEmpty && 
                  System.nanoTime < endTime  &&
                  ComputationBound.advanceDeferredQueue(endTime)
            ) { }
         else
            while(ref.get().isEmpty &&
                 ComputationBound.advanceDeferredQueue(System.nanoTime + 100000)
            ) { }
         ref.get.map{ r => 
             val afterWait = Duration(System.nanoTime, NANOSECONDS)
             op(r).progress(timeout - (afterWait - beforeWait)) 
         }.getOrElse(this)
      
  override def flatMap[S](f: T => ComputationBound[S]): ComputationBound[S] =
        Wait(ref, x => op(x) flatMap f)

  override def flatMapTry[S](f: Try[T]=>ComputationBound[S]): ComputationBound[S] =
        Wait(ref, x => op(x).flatMapTry(f))       

  override def map[S](f: T=>S): ComputationBound[S] =
        Wait(ref, x => op(x).map(f) )

}

