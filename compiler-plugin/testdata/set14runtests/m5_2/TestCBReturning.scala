package cpstest

import org.junit.{Test,Ignore}
import org.junit.Assert._

import scala.util.*
import scala.util.control.NonFatal
import scala.util.control.NonLocalReturns.*


import cps.*
import cps.testconfig.given



class TestCBSReturning:


  @Test def testReturningFromTry(): Unit = {
     //implicit val printCode = cps.macros.flags.PrintCode
     //implicit val printTree = cps.macros.flags.PrintTree
     //implicit val debugLevel = cps.macros.flags.DebugLevel(15)
     val c = async[ComputationBound] {
         returning {
            try   
              await(T1.cbi(2))
              throwReturn(2)
            catch 
              case NonFatal(ex) =>
                8
         }
     }
     val r = c.run()
     println(s"r == $r")
     r match
       case Failure(ex) => ex.printStackTrace()
       case Success(_) =>
     assert(r.isSuccess)
     r match
       case Success(v) => assert(v == 2)
       case Failure(ex) => throw ex
  }


  
