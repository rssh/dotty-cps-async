package cps


import org.junit.{Test,Ignore}
import org.junit.Assert._

import scala.language.implicitConversions
import scala.language.postfixOps
import scala.concurrent._
import scala.concurrent.duration._
import scala.util._

import cps.monads.given

import cps.testconfig.given

class TestCBSFutureIntegration:


  import scala.concurrent.ExecutionContext.Implicits.global

  @Test def futureBasic2(): Future[Try[Unit]] =
     def fun(x:Int):Future[Int] =
       Future successful (x+1)
     
     val c = async[ComputationBound]{
       val a = await(fun(10))
       a
     }
     
     //assert(c.run()==Success(11))
     c.runTicks(1 minute).map{ r =>
         assert( r == 11 )
         Success(())
     }
 





