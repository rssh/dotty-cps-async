package cps

import org.junit.{Test,Ignore}
import org.junit.Assert._

import scala.util._


class TestCBS1ShiftTryMonad:

  @Test def testMapTry(): Unit = 
     //implicit val printCode = cps.macroFlags.PrintCode
     //implicit val debugLevel = cps.macroFlags.DebugLevel(20)
     val c = async[ComputationBound]{
            val q = summon[CpsTryMonad[ComputationBound]].mapTry(T1.cbi(2)){
               case Success(x) => x + await(T1.cbi(3))
               case Failure(ex) => 0
            }
            await(q)
     }
     val r = c.run()
     assert(c.run() == Success(5))

  @Test def testMapTry1(): Unit = 
     //implicit val printCode = cps.macroFlags.PrintCode
     //implicit val debugLevel = cps.macroFlags.DebugLevel(20)
     val c: ComputationBound[Int] = async[ComputationBound]{ 
            val q = summon[CpsTryMonad[ComputationBound]].mapTry(T1.cbi(2)){ v =>
              v match
                 case Success(x) => x + await(T1.cbi(3))
                 case Failure(ex) => 0
            }
            await(q)
     }
     val r = c.run()
     assert(c.run() == Success(5))





