package cps

import org.junit.{Test,Ignore}
import org.junit.Assert._

import scala.quoted._
import scala.util.Success
import scala.util.Failure


class TestBS1ShiftMap:


  @Test def tesGetOrElse1(): Unit = 
     //implicit val printCode = cps.macroFlags.PrintCode
     //implicit val debugLevel = cps.macroFlags.DebugLevel(20)
     val c = async[ComputationBound]{
        val v = Map(1->"1",3->"3")
        v.getOrElse(2,await(T1.cbs("AAA")))
     }
     assert(c.run() == Success("AAA"))

  @Test def testMap1(): Unit = 
     //implicit val printCode = cps.macroFlags.PrintCode
     //implicit val debugLevel = cps.macroFlags.DebugLevel(20)
     val c = async[ComputationBound]{
        Map(1->1,2->2,3->3).map{ x =>
           await(T1.cbi(3)) + x._1
        }.toSeq
     }
     assert(c.run() == Success(Seq(4,5,6)))

  @Test def testMapApplyCompanion(): Unit = 
     //implicit val printCode = cps.macroFlags.PrintCode
     //implicit val printTree = cps.macroFlags.PrintTree
     //implicit val debugLevel = cps.macroFlags.DebugLevel(20)
     val c = async[ComputationBound]{
        Map(1->"one",2->await(T1.cbs("two")),3->"three")
     }  
     val r = c.run().get
     assert(r(1) == "one")
     assert(r(2) == "two")

  @Test def testMapForeach(): Unit = 
     //implicit val printCode = cps.macroFlags.PrintCode
     //implicit val debugLevel = cps.macroFlags.DebugLevel(20)
     var sx = 0
     var sy = 0
     val c = async[ComputationBound]{
        Map(1->1,2->1,3->1).foreachEntry{ (x, y) =>
           sx = sx + await(T1.cbi(x))
           sy = sy + await(T1.cbi(y))
        }
     }
     val r = c.run().get
     assert(sx == (1+2+3))
     assert(sy == (1+1+1))



