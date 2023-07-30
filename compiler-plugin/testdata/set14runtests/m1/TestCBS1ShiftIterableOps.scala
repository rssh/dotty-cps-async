package cps

import org.junit.{Test,Ignore}
import org.junit.Assert._

import scala.quoted._
import scala.util.Success
import scala.util.Failure
import cps.plugin.annotation.CpsDebugLevel

import cps.testconfig.given


@CpsDebugLevel(20)
class TestBS1ShiftIterableOps:


  @Test def testCollectFirst(): Unit =
     //implicit val printCode = cps.macroFlags.PrintCode
     //implicit val printTree = cps.macroFlags.PrintTree
     //implicit val debugLevel = cps.macroFlags.DebugLevel(20)
     val c = async[ComputationBound]{
          val c = List(1,2,3,4)
          val r: Option[Int] = c.collectFirst{ case x if x > 2 => x + await(T1.cbi(1)) }
          //val r: Option[Int] = await(T1.cbt(Some(4)))
          r
     }
     assert(c.run() == Success(Some(4)))



