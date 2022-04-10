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
          cancelWaits.forEach( (k,v) =>
            v.nn.tryFailure(cancelException)
          )  
          childScopes.forEach( (k,v) =>
            v.nn.cancel()
          )
          state.set(CANCELLED)
      }  
      
      def onFinish(callback: ()=>Unit): Unit =
        onFinishCallbacks.addFirst(callback)

      private val cancelWaits: ConcurrentHashMap[Promise[?],Promise[?]] = new ConcurrentHashMap() 
      private val childScopes: ConcurrentHashMap[FutureScopeContext, FutureScopeContext] = new ConcurrentHashMap()   

      private val onFinishCallbacks: ConcurrentLinkedDeque[()=>Unit] = new ConcurrentLinkedDeque()
      
      private[futureScope] def finish(): Unit =
        while{
          val e = onFinishCallbacks.pollFirst()
          if (e == null) {
            false
          } else {
            e.apply()
            true       
          }
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