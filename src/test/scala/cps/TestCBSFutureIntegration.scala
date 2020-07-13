package cps

import org.junit.{Test,Ignore}
import org.junit.Assert._

import scala.language.implicitConversions
import scala.concurrent._
import scala.util._

   
given cbsFutureConversion(using ExecutionContext) as CpsMonadConversion[Future,ComputationBound] = 
   new CpsMonadConversion[Future, ComputationBound] {
     override def apply[T](mf: CpsMonad[Future], mg: CpsMonad[ComputationBound], ft:Future[T]):
        ComputationBound[T] =
           ComputationBound.asyncCallback( listener => ft.onComplete(listener) )
   }


class TestCBSFutureIntegration:


  import scala.concurrent.ExecutionContext.Implicits.global

  @Test def futureBasic2(): Unit =
     def fun(x:Int):Future[Int] =
       Future successful (x+1)
     
     val c = async[ComputationBound]{
       val a = await(fun(10))
       a
     }
     
     //val af = fun(10)
     //val cb = summon[Conversion[Future[Int],ComputationBound[Int]]].apply(af)
     assert(c.run()==Success(11))




