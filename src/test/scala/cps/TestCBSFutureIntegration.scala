package cps

import org.junit.{Test,Ignore}
import org.junit.Assert._

import scala.language.implicitConversions
import scala.concurrent._
import scala.util._

given future2ComputationBound[T](using ExecutionContext) as Conversion[Future[T],ComputationBound[T]] = 
   future => 
          ComputationBound.asyncCallback( listener => future.onComplete(listener) )
   


class TestCBSFutureIntegration:

  //given future2ComputationBound[T] as Conversion[Future[T],ComputationBound[T]] = ???

  import scala.concurrent.ExecutionContext.Implicits.global

  @Test @Ignore def futureBasic2(): Unit =
     def fun(x:Int):Future[Int] =
       Future successful (x+1)
     
     //val c = async[ComputationBound]{
     //  val a = await(fun(10))
     //  a
     //}
     
     //val af = fun(10)
     //val cb = summon[Conversion[Future[Int],ComputationBound[Int]]].apply(af)
     //assert(c.run()==Success(11))




