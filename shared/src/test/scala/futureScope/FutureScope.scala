package futureScope

import scala.concurrent.*
import scala.util.*
import java.util.concurrent.atomic.*
import java.util.concurrent.ConcurrentLinkedDeque
import java.util.concurrent.ConcurrentHashMap


import cps.*
import cps.util.*

import org.junit.{Test,Ignore}
import org.junit.Assert._

class CancelException(message:String = "cancel") extends RuntimeException(message)

object FutureScope {
    
    type FutureScope[+T] = Future[T]

    class FutureScopeContext(ec: ExecutionContext) extends CpsMonadContext[FutureScope] {

      final val INITIAL=0 
      final val CANCELLING=1
      final val CANCELLED=2

      val state = new AtomicInteger(INITIAL) 
      private val cancelWaits: ConcurrentHashMap[Promise[?],Promise[?]] = new ConcurrentHashMap() 
      private val childScopes: ConcurrentHashMap[FutureScopeContext, FutureScopeContext] = new ConcurrentHashMap()   
      private val onFinishCallbacks: AtomicReference[List[()=>Unit]] = new AtomicReference(Nil)


      override def adoptAwait[A](fa: FutureScope[A]):FutureScope[A] =
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



    class FutureScopeMonad(using ec: ExecutionContext) extends CpsTryMonad[Future] {

        override type Context = FutureScopeContext


        type F[A] = FutureScope[A]

        def apply[T](f: Context => F[T]):F[T] = {
          val c = new FutureScopeContext(ec)
          f(c).map{ t =>
                c.finish()
                t
          }
        }

        def pure[A](a:A):F[A] = Future successful a

        def map[A,B](fa: F[A])(f: A=>B): F[B] =
          fa.map(f)
      
        def flatMap[A,B](fa:F[A])(f: A=>F[B]):F[B] =
          fa.flatMap(f)
    
        def error[A](e: Throwable): F[A] =
          Future.failed(e)
       
        override def mapTry[A,B](fa:F[A])(f: Try[A]=>B): F[B] =
          fa.transform{ v => Success(f(v)) }
    
        def flatMapTry[A,B](fa:F[A])(f: Try[A]=>F[B]): F[B] =
          fa.transformWith{ v => f(v) }

    }

    given futureScopeMonad(using ec: ExecutionContext): FutureScopeMonad = 
              FutureScopeMonad(using ec)
}


class TestFutureScopeBase {

  import scala.concurrent.ExecutionContext.Implicits.global

  import FutureScope.{*,given}

  def foo(x:Int):Future[Int] =
    Future successful x+1

  @Test def testFutureScopeBase() = {

      val f = async[FutureScope] {
         val x1 = await(foo(1))
         summon[FutureScopeContext].onFinish(() => x1+1)
         x1
      }
      FutureCompleter(f.map(x => assert(x==2)))
  }


}