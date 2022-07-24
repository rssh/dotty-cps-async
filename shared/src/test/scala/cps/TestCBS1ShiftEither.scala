package cps

import org.junit.{Test,Ignore}
import org.junit.Assert._

import scala.util._
import cps.testconfig.given

class TestBS1ShiftEither:


  @Test def testEitherGetOrElse0(): Unit = 
     //implicit val printCode = cps.macroFlags.PrintCode
     //implicit val debugLevel = cps.macroFlags.DebugLevel(20)
     val c = async[ComputationBound]{
        val a: Either[String,Int] = Right(0)
        a.getOrElse{
            await(T1.cbi(2))
        }
     }
     assert(c.run() == Success(0))

  @Test def testEitherGetOrElse1(): Unit = 
     //implicit val printCode = cps.macroFlags.PrintCode
     //implicit val debugLevel = cps.macroFlags.DebugLevel(20)
     val c = async[ComputationBound]{
        val a: Either[String,Int] = Left("A")
        a.getOrElse{
            await(T1.cbi(2))
        }
     }
     assert(c.run() == Success(2))

  @Test def testEitherLeftForAll1(): Unit = 
     //implicit val printCode = cps.macroFlags.PrintCode
     //implicit val debugLevel = cps.macroFlags.DebugLevel(20)
     val c = async[ComputationBound]{
        val a: Either[String,Int] = Left("A")
        a.left.forall{ _ == await(T1.cbs("A")) }
     }
     assert(c.run() == Success(true))




