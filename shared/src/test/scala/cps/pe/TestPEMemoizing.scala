package cps.pe

import org.junit.{Test,Ignore}
import org.junit.Assert._

import scala.quoted._
import scala.util.Success

import cps._
import cps.testconfig.given
import cps.util.FutureCompleter

import scala.concurrent.ExecutionContext.Implicits.global

class TestPEMemoizing:

  def qqq = 1
   
  @Test def testMemoizingBasic() = 
     var ref = 0
     val effect = PureEffect.delay[Int]({ ref = ref+1; ref})
     val memoized = effect.memoize()
     val c = (for{
       v <- memoized
       x <- v
       y <- v
     } yield (v, x, y))
     val future = c.unsafeRunFuture().map{
       case (v,x,y) =>
           assert(x == y)
           assert(x == 1)
           assert(ref == 1)
     }
     FutureCompleter(future)

  @Test def test2ValsInBlock(): Unit = 
     var ref = 0
     val effect = PureEffect.delay[Int]({ ref = ref+1; ref})
     val c = async[PureEffect]{
         val m = effect.memoize()
         val v1 = await(m)
         val x = await(v1)
         val y = await(v1)
         assert(x == y)
         assert(ref == 1)
     }
     val future = c.unsafeRunFuture()
     FutureCompleter(future)





