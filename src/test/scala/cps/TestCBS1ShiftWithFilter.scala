package cps

import org.junit.{Test,Ignore}
import org.junit.Assert._

import scala.util.Success
import scala.util.Failure


class TestCBS1ShiftWithFilter:

  @Test def testSimple1: Unit =
     implicit val printCode = cps.macroFlags.PrintCode
     //implicit val printTree = cps.macroFlags.PrintTree
     //implicit val debugLevel = cps.macroFlags.DebugLevel(20)
     val c = async[ComputationBound]{
          for{ x <- 1 to 10 if (x%2) == await(T1.cbi(0)) 
             } yield (x)
     }
     val r = c.run().get
     println(s"withFilter: r==$r")
     assert(r(0) == 2 )

/*
  @Test def testSimple2: Unit =
     implicit val printCode = cps.macroFlags.PrintCode
     //implicit val printTree = cps.macroFlags.PrintTree
     //implicit val debugLevel = cps.macroFlags.DebugLevel(20)
     val c = async[ComputationBound]{
          for{ x <- 1 to 10 if (x%2) == await(T1.cbi(0)) 
               y <- 1 to 10 if (x%2) == await(T1.cbi(1)) 
          } yield (x,y)
     }
     val r = c.run().get
     assert(r(0) == (0,1) )
*/




