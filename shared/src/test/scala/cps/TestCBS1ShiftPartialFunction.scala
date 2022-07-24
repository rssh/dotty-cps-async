package cps

import org.junit.{Test,Ignore}
import org.junit.Assert._

import scala.util._
import cps.testconfig.given


class TestCBS1ShiftPartialFunction:

  def qqq: Int = 3

  @Test def testAndThen1(): Unit = 
     //implicit val printCode = cps.macroFlags.PrintCode
     //implicit val debugLevel = cps.macroFlags.DebugLevel(20)
     val c = async[ComputationBound]{
        val n0: PartialFunction[Int,Int] = { case (x:Int) if x%2 == 0 => 1 }
        n0.andThen{ case (x:Int) if x%3 == 0 => await(T1.cbi(2)) }(2)
     }
     val x = c.run()
     assert(x.isFailure)


  @Test def testAndThen2(): Unit = 
     //implicit val printCode = cps.macroFlags.PrintCode
     //implicit val debugLevel = cps.macroFlags.DebugLevel(20)
     val c = async[ComputationBound]{
        val n0: PartialFunction[Int,Int] = { case (x:Int) if x%2 == 0 => 3 }
        n0.andThen{ case (x:Int) if x%3 == 0 => await(T1.cbi(2)) }(2)
     }
     val x = c.run()
     assert(x == Success(2) )


  @Test def testAndThen3(): Unit = 
     //implicit val printCode = cps.macroFlags.PrintCode
     //implicit val debugLevel = cps.macroFlags.DebugLevel(20)
     val c = async[ComputationBound]{
        val n0: PartialFunction[Int,Int] = { case (x:Int) if x%2 == 0 => 1 }
        n0.andThen{ x => x + await(T1.cbi(2)) }(2)
     }
     val x = c.run()
     assert(x == Success(3))

  @Test def testBeforeOrElse0(): Unit = 
     //implicit val printCode = cps.macroFlags.PrintCode
     //implicit val debugLevel = cps.macroFlags.DebugLevel(20)
     val c = async[ComputationBound]{
        val n0: PartialFunction[Int,Int] = { case (x:Int) if x%2 == 0 => 1 }
        val n1: PartialFunction[Int,Int] = { case (x:Int) if x%2 == 1 => 2 }
        val f = n0.orElse(n1) 
        f(2)
     }
     val x = c.run()
     assert(x == Success(1))



