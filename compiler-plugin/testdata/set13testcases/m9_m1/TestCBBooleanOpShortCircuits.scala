package cps

import org.junit.{Test,Ignore}
import org.junit.Assert._

import scala.util.*


import cps.*
import cps.plugin.annotation.*
import cps.macros.flags.UseCompilerPlugin
given UseCompilerPlugin.type = UseCompilerPlugin

//@CpsDebugLevel(20)
class TestCBBooleanOpShortCircuits {

  
  @Test def testLogicalAnd_0_0(): Unit = {
     //implicit val printCode = cps.macroFlags.PrintCode
     //implicit val debugLevel = cps.macros.flags.DebugLevel(5)
     var x = false
     var y = false
     var xCalled = false
     var yCalled = false 
     val c = async[ComputationBound] {
        { xCalled=true; x } && { yCalled=true; y } 
     }
     val r = c.run()
     assert(r.isSuccess)
     r match
       case Success(v) => 
          assert(xCalled == true)
          assert(yCalled == false)
       case Failure(ex) => throw ex
  }



}