package cps

import org.junit.{Test,Ignore}
import org.junit.Assert._

import scala.concurrent._
import scala.util._

given future2ComputationBound[T](using ExecutionContext) as Conversion[Future[T],ComputationBound[T]] = 
   future => 
          ComputationBound.asyncCallback( listener => future.onComplete(listener) )
   


class TestCBSFutureIntegration:


  @Test def futureBasic2(): Unit =
     def fun(x:Int):Future[Int] =
       import scala.concurrent.ExecutionContext.Implicits.global
       Future successful (x+1)
     val c = async[ComputationBound]{
       //import scala.concurrent.ExecutionContext.Implicits.global
       val a = await(fun(10))
       a
     }
     assert(c.run()==Success(11))




