package cps

import org.junit.{Test,Ignore}
import org.junit.Assert._

import scala.util.Success
import scala.util.Failure

import cps.plugin.annotation.CpsDebugLevel
import cps.macros.flags.UseCompilerPlugin
given UseCompilerPlugin.type = UseCompilerPlugin


@CpsDebugLevel(20)
class TestCBS1ShiftWithFilterM6min1:


  @Test def testSimple2l_p3: Unit =
     //implicit val printCode = cps.macroFlags.PrintCode
     //implicit val printTree = cps.macroFlags.PrintTree
     //implicit val debugLevel = cps.macroFlags.DebugLevel(20)
     import cps.runtime._
     val c = async[ComputationBound]{
                       val x = 1
                       await(RangeAsyncShift[Range.Inclusive]().withFilter(20 to 30, ComputationBoundAsyncMonad)(
                                  y => T1.cbi(1).map( a => a == y%2 )  )
                                .map(y => x+y)
                       )
     }
     val r = c.run().get



