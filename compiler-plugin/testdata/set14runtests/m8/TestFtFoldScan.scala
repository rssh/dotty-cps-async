package cps

import org.junit.{Test,Ignore}
import org.junit.Assert._

import scala.concurrent.*
import scala.concurrent.duration.*
import scala.concurrent.ExecutionContext.Implicits.global


import cps.*
import cps.stream.*
import cps.monads.{*, given}
import cps.plugin.annotation.CpsDebugLevel

import cps.testconfig.given

class TestFbFoldScan:

  //@CpsDebugLevel(20)
  @Test def testFoldAsync() =
    val check = async[Future] {

      val stream = asyncStream[AsyncList[Future, Int]]{ out =>
        for(i <- 1 to 10) 
          out.emit(i)
      }
      val sumFuture = stream.fold(0)((s,e)=>s+await(Future successful e))
      val sum = await(sumFuture)
      //val sum1 = (1 to 10).sum
      assert(sum == 55)
    }
    val r = Await.result(check, 5.seconds)
  
