package cps

import org.junit.{Test,Ignore}
import org.junit.Assert._

import scala.quoted._
import scala.util.Success


class TestBS1ShiftCollectionOps:


  @Test def testMapList(): Unit = 
     implicit val printCode = cps.macroFlags.PrintCode
     implicit val debugLevel = cps.macroFlags.DebugLevel(20)
     val c = async[ComputationBound]{
        List(1,2,3).map{ x =>
           await(T1.cbi(3)) + x
        }
     }
     assert(c.run() == Success(List(4,5,6)))

  @Test def testMapSeq(): Unit = 
     val c = async[ComputationBound]{
        Seq(1,2,3).map{ x =>
           await(T1.cbi(3)) + x
        }
     }
     assert(c.run() == Success(Seq(4,5,6)))

/*
  @Test def testMapSet(): Unit = 
     val c = async[ComputationBound]{
        Set(1,2,3).map{ x =>
           await(T1.cbi(3)) + x
        }
     }
     assert(c.run() == Success(Set(4,5,6)))
*/
  


