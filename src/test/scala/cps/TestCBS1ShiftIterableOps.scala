package cps

import org.junit.{Test,Ignore}
import org.junit.Assert._

import scala.quoted._
import scala.util.Success
import scala.util.Failure


class TestBS1ShiftIterableOps:


  @Test def testCollectFind(): Unit =
     implicit val printCode = cps.macroFlags.PrintCode
     //implicit val debugLevel = cps.macroFlags.DebugLevel(20)
     val c = async1[ComputationBound]{
          //val c = List(1,2,3,4)
          //val r = c.collectFirst{ case x if x > 0 => x + await(T1.cbi(1)) > 3 }
          //r
          List(1,2,3,4).collectFirst{ case x if x > 0 => x + await(T1.cbi(1)) > 3 }
     }



