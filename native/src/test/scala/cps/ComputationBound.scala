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


trait ComputationBound[+T] {
 
  def run(timeout: Duration = Duration.Inf): Try[T] =
    progress(timeout) match 
      case Done(t) => Success(t)
      case Error(e) => Failure(e)
      case _ => Failure(new TimeoutException())

  def progress(timeout: Duration): ComputationBound[T]

  def runTicks(timeout: Duration): Future[T] =
    ???

  def checkProgress(timeout: Duration = Duration.Inf): Either[Try[T],ComputationBound[T]] =
        progress(timeout) match
           case Done(t) => Left(Success(t))
           case Error(e) => Left(Failure(e))
           case other => Right(other)               
  
  def map[S](f:T=>S): ComputationBound[S] =
     flatMap( x => Done(f(x)) )

  def flatMap[S](f: T=> ComputationBound[S]): ComputationBound[S]

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

   private val externalAsyncNotifier = new java.lang.Object()

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
      if (nFinished == 0) then
        val timeToWait = math.min(waitQuant.toNanos, endNanos - System.nanoTime)
        val timeToWaitMillis = (timeToWait nanos).toMillis
        if (timeToWaitMillis > 0) then 
            externalAsyncNotifier.synchronized {
                 externalAsyncNotifier.wait(timeToWaitMillis)
            }      
      nFinished > 0
   }

  

}

implicit object ComputationBoundAsyncMonad extends CpsAsyncMonad[ComputationBound] with CpsMonadInstanceContext[ComputationBound]  {

   type WF[T] = ComputationBound[T]

   def pure[T](value:T): ComputationBound[T] = ComputationBound.pure(value)

   def finalAwait[T](t:ComputationBound[T]):Try[T] = t.run()

   def map[A,B](fa:ComputationBound[A])(f: A=>B): ComputationBound[B] = fa.map(f)

   def flatMap[A,B](fa:ComputationBound[A])(f: A=>ComputationBound[B]):ComputationBound[B] = 
            fa.flatMap(f) 

   def error[T](e: Throwable):ComputationBound[T] = Error[T](e)

   override def mapTry[A,B](fa: ComputationBound[A])(f:Try[A]=>B): ComputationBound[B] =
     Thunk(() => Done(f(fa.run())))

   override def flatMapTry[A,B](fa: ComputationBound[A])(f:Try[A]=>ComputationBound[B]): ComputationBound[B] =
     Thunk(() => f(fa.run()))


   override def restore[A](fa: ComputationBound[A])(fx:Throwable => ComputationBound[A]): ComputationBound[A] = Thunk(() => {
         fa.run() match 
            case Success(a) => Done(a)
            case Failure(ex) => fx(ex)
       })

   override def withAction[A](fa:ComputationBound[A])(action: =>Unit):ComputationBound[A] = Thunk(() => {
         val r = fa.run()  
         action
         r match {
           case Success(a) => Done(a)
           case Failure(ex) => Error(ex)
         }
       })

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
     
}

case class Done[T](value:T) extends ComputationBound[T]:

  override def progress(timeout: Duration): ComputationBound[T] = this

  override def flatMap[S](f: T=>ComputationBound[S] ): ComputationBound[S] =
     Thunk( () => f(value) )


case class Error[T](e: Throwable) extends ComputationBound[T]:

  override def progress(timeout: Duration): ComputationBound[T] = this

  override def flatMap[S](f: T=> ComputationBound[S]): ComputationBound[S] =
     this.asInstanceOf[ComputationBound[S]]


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

  override def map[S](f: T=>S): ComputationBound[S] =
        Wait(ref, x => op(x).map(f) )

}

