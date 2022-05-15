package futureScope

import java.util.concurrent.atomic.*
import java.util.concurrent.ConcurrentLinkedQueue
import scala.concurrent.*
import scala.util.*
import cps.*
import cps.monads.{*, given}
import cps.stream.*


trait EventFlow[E] {

   def events: AsyncIterator[Future, E]

   def post(e: E): Unit =
      postTry(Success(e))

   def postFailure(e: Throwable): Unit =
      postTry(Failure(e))

   def postTry(v: Try[E]): Unit

   def finish(): Unit

}

object EventFlow {

   def apply[E]()(using ec: ExecutionContext): EventFlow[E] =
      new DefaultEventFlowBase[E](ec) 

}

class DefaultEventFlowBase[E](ec: ExecutionContext) extends EventFlow[E] {

   val outQueue: ConcurrentLinkedQueue[Try[E]] = new ConcurrentLinkedQueue()
   val finished: AtomicBoolean = new AtomicBoolean(false)
   val nextPromise: AtomicReference[Promise[Option[E]]|Null] = new AtomicReference(null) 

   given ExecutionContext = ec

   class EventsIterator extends AsyncIterator[Future, E] {

      override def next: Future[Option[E]] = {
         val v = outQueue.poll()
         if v != null then 
            toOut(v)
         else
            if finished.get() then
               val v = outQueue.poll()  // can be changed after closed reading
               if (v != null) then
                  toOut(v)
               else
                  // here if event is lost, than it was posted after close
                  Future successful None
            else
               val p: Promise[Option[E]] = Promise()
               val np = nextPromise.compareAndExchange(null,p)
               if np == null then
                  val v = outQueue.poll()
                  if (v != null) then 
                     // TODO: prove, that pther instance of case can not change nextPromise
                     //  between 
                     nextPromise.compareAndExchange(p,null)
                     toOut(v)
                  else p.future
               else 
                  // we call next before resolvig, 
                  //  TODO: hanelw error and use flatMapTry ?
                  np.future.flatMap{ _ => next }
      }
         
      private def toOut(v: Try[E]): Future[Option[E]] = {
         v match
            case Success(x) => Future successful Some(x)
            case Failure(ex) => Future failed ex
      }

   }

   override val events: AsyncIterator[Future,E] = new EventsIterator()

   def postTry(e: Try[E]): Unit = {
      if finished.get() then
         throw new IllegalStateException("eventFlow is finished")
      while {
         val p = nextPromise.get()
         if (p == null) then
             outQueue.add(e)
             val p1 = nextPromise.get()
             if (p1 != null) then
                 touchOut(p1)
             false
         else if nextPromise.compareAndSet(p, null) then
             val out = withOption(e)
             if p.tryComplete(out) then
                false
             else
                // actually it's impossible, because we set null to prevent read from
                //  other post
                true
         else
            true 
      } do()
   }

   def finish(): Unit = {
      finished.set(true)
   }

   private def touchOut(p0: Promise[Option[E]]):Unit = {
      var v = outQueue.poll()
      if (v != null) then
         var p = p0
         while{
            val np = nextPromise.compareAndExchange(p,null)
            if (np == null) && p.tryComplete(withOption(v)) then 
               false 
            else
               p = np.nn
               true
         } do ()
   }

   private def withOption[T](e:Try[T]):Try[Option[T]] =
      e match
         case Success(x) => Success(Some(x))
         case Failure(ex) => Failure(ex)

}