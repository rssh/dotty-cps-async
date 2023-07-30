package cps

import org.junit.{Test,Ignore}
import org.junit.Assert._

import scala.util._

import cps.macros.flags.UseCompilerPlugin
given UseCompilerPlugin.type = UseCompilerPlugin


class TestCBS1ShiftTryMonad:



  @Test def testFlatMapTry(): Unit = 
     //implicit val printCode = cps.macros.flags.PrintCode
     //implicit val debugLevel = cps.macros.flags.DebugLevel(20)
     val c = async[ComputationBound]{
          val q = summon[CpsTryMonad[ComputationBound]].flatMapTry(T1.cbi(2)){ v =>
              v match
                case Success(x) => T1.cbi(x + await(T1.cbi(3)))
                case Failure(ex) => T1.cbi(0)
          }
          await(q)
     }
     val v = c.run()
     //println(s"TestFlatMapTry: v=$v")
     assert(c.run() == Success(5))



