package futureScope

import scala.concurrent.*
import scala.util.*
import scala.concurrent.ExecutionContext.Implicits.global

import cps.*
import cps.monads.{*,given}
import cps.util.*

import java.util.concurrent.CancellationException

import org.junit.{Test,Ignore}
import org.junit.Assert._

case class ScopeCancellationException(message:String = "cancel", ex: Throwable|Null = null) extends CancellationException(message) {

   if ex != null then
      initCause(ex)

}

class FutureScope(ec: ExecutionContext) extends CpsMonadContextProvider[Future] {
  
   override type Context = FutureScopeContext

   override def  contextualize[A](fa: Context => Future[A]): Future[A] = {
      val fsc = new FutureScopeContext(ec)
      fa(fsc).map{ x => 
        fsc.finish()
        x
      }     
   }
   

}
    
val Scope = FutureScope(ExecutionContext.Implicits.global)
 

class TestFutureScopeBase {

  import scala.concurrent.ExecutionContext.Implicits.global


  def foo(x:Int):Future[Int] =
    Future successful x+1

  @Test def testFutureScopeBase() = {

      val f = async[Future].in(Scope) {
         val x1 = await(foo(1))
         summon[FutureScopeContext].onFinish(() => x1+1)
         x1
      }
      FutureCompleter(f.map(x => assert(x==2)))
  }


}
