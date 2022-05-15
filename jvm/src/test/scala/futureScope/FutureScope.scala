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


class FutureScope(ec: ExecutionContext) extends CpsMonadContextProvider[Future] {
  
   override type Context = FutureScopeContext

   override def  contextualize[A](fa: Context => Future[A]): Future[A] = {
      val fsc = new FutureScopeContext(ec)
      fsc.run(fa)
   }
   

}

object FutureScope {

   def spawn[A](using fsc:FutureScopeContext)(f: FutureScopeContext ?=> A, executionContext: ExecutionContext = fsc.executionContext): Future[A] =
      summon[FutureScopeContext].spawn(f,executionContext)

   def spawnAsync[A](using fsc:FutureScopeContext)(f: FutureScopeContext => Future[A], executionContext: ExecutionContext = fsc.executionContext ) =
      summon[FutureScopeContext].spawnAsync(f, executionContext)

}

    
val Scope = FutureScope(ExecutionContext.Implicits.global)
 

class TestFutureScopeBase {

  import scala.concurrent.ExecutionContext.Implicits.global


  def foo(x:Int):Future[Int] =
    Future successful x+1

  @Test def testFutureScopeBase() = {

      var y = 0
      val f = async[Future].in(Scope) {
         val x1 = await(foo(1))
         summon[FutureScopeContext].onFinish{y = x1+1}
         x1
      }
      FutureCompleter(f.map(x => assert(x==2)))
  }


}
