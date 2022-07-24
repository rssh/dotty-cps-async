package cps

import org.junit.{Test,Ignore}
import org.junit.Assert._

import scala.quoted._
import scala.util.Success

import cps.testconfig.given

class TestCBS1Def:


  //  local packages are not supported by dottt 
  //@Test def packageDef_00(): Unit = 

  @Test def classDef_00(): Unit = 
     val c = async[ComputationBound]{
        val x = 1
        class LocalClass {
          val y = 2
        }
        val z = new LocalClass()
        z.y
     }
     assert(c.run() == Success(2))

  @Test def classDef_01(): Unit = 
     //implicit val printCode = cps.macroFlags.PrintCode
     //implicit val debugLevel = cps.macroFlags.DebugLevel(20)
     val c = async[ComputationBound]{
        val x = await(T1.cbi(1))
        class LocalClass {
          val y = 2 
        }
        val z = new LocalClass()
        z.y + x
     }
     assert(c.run() == Success(3))

  @Test def classDef_01d(): Unit = 
     val c = async[ComputationBound]{
        val x = await(T1.cbi(1))
        class LocalClass {
          val y = 2 + x
        }
        val z = new LocalClass()
        z.y 
     }
     assert(c.run() == Success(3))

  @Test def defDef_01(): Unit = 
     //implicit val printCode = cps.macroFlags.PrintCode
     //implicit val debugLevel = cps.macroFlags.DebugLevel(20)
     val c = async[ComputationBound]{
        val x = await(T1.cbi(1))
        def localFunction() = 3
        x + localFunction()
     }
     assert(c.run() == Success(4))


