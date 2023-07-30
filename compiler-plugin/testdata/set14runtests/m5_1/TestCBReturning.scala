package cpstest

import org.junit.{Test,Ignore}
import org.junit.Assert._

import scala.util.*
import scala.util.control.NonFatal
import scala.util.control.NonLocalReturns.*


import cps.*
import cps.testconfig.given



class TestCBSReturning:


  @Test def testSyncNoReturn(): Unit = {
    //implicit val printCode = cps.macros.flags.PrintCode
    //implicit val debugLevel = cps.macros.flags.DebugLevel(10)
    val c = async[ComputationBound] {
      returning {
        3
      }
    }
    val r = c.run()
     assert(r.isSuccess)
     r match
       case Success(v) => assert(v == 3)
       case Failure(ex) => throw ex
  }






  
