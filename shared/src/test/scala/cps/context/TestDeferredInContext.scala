package cps.context

import scala.util.*
import scala.util.control.*
import scala.collection.mutable.Stack

import cps.*
import cps.testconfig.given

import org.junit.{Test,Ignore}
import org.junit.Assert.*


class DeferredDestructorsContext extends CpsMonadContext[ComputationBound] {

  // non-reentrable for this example.
  //val  deferred: ConcurrentLinkedDeque[()=>Unit] = new ConcurrentLinkedDeque()
  
  val  deferred = Stack[()=>Unit]()

  def  deferr(f: =>Unit): Unit =
    deferred.push(() => f)

  def  cleanup(): Option[Throwable] = 
  {
    var errors: Seq[Throwable] = Seq.empty
    while(!deferred.isEmpty) {
       val f = deferred.pop()
       try {
         f()
       }catch{
         case NonFatal(ex) =>
           errors = errors :+ ex
       }
    }
    if errors.isEmpty then
      None
    else 
      val first=errors.head
      for(e <- errors.tail) {
        first.addSuppressed(e)
      }
      Some(first)
  }  

  def  withContext[A](f: ComputationBound[A], m: CpsTryMonad[ComputationBound]): ComputationBound[A] =
    m.flatMapTry(f){ r =>
      r match
        case Success(x) =>
                cleanup() match
                   case None => m.pure(x)
                   case Some(ex) => m.error(ex)
        case Failure(ex) =>
                cleanup().foreach(ex.addSuppressed(_))
                m.error(ex)                      
    } 

  // not intercept awaits.  
  override def adoptAwait[A](fa: ComputationBound[A]):ComputationBound[A] = fa

}

object DeferredProvider extends CpsMonadContextProvider[ComputationBound] {

   type Context = DeferredDestructorsContext

   def contextualize[T](f: Context => ComputationBound[T] ): ComputationBound[T] =
      val ctx = new DeferredDestructorsContext()
      ctx.withContext(f(ctx), summon[CpsTryMonad[ComputationBound]])

}


class TestDeferredInContextMonad {


  @Test def testSimpleContext(): Unit = 
      var x = 0
      val c = async.in(DeferredProvider){ scope ?=>
        scope.deferr{ x = 1 } 
        1
      }
      val r = c.run()
      assert (x == 1)
      assert(r == Success(1)) 
  

}
