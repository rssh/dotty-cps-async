package cps

import org.junit.{Test,Ignore}
import org.junit.Assert._

import scala.quoted._
import scala.util._

import cps.*
import cps.automaticColoring.{*,given}
import scala.language.implicitConversions
import scala.annotation.implicitAmbiguous

class TestDisableAutomaticColoring:

  def passInt(x:Int):Int = x


  @Test def testEnableAutomaticColoring(): Unit = {
     val c = async {
         val x = T1.cbi(1)
         passInt(x)
     }
     val r = c.run()
     assert(r == Success(1))
  }



  @Test def testEnableAutomaticColoringTypeCheck(): Unit = {
     val code = 
     """
       val c = async {
         val x = T1.cbi(1)
         passInt(x)
       }
     """
     assert(compiletime.testing.typeChecks(code))
  }

 
  @Test def testDisableAutomaticColoringTypeCheck(): Unit = {
     val code = 
     """
       @implicitAmbiguous("automatic coloring is disabled")
       implicit val dummy1: AutomaticColoringTag[ComputationBound] = new AutomaticColoringTag[ComputationBound] {}
       implicit val dummy2: AutomaticColoringTag[ComputationBound] = new AutomaticColoringTag[ComputationBound] {}
       val c = async {
         val x = T1.cbi(1)
         passInt(x)
       }
     """
     val errors = compiletime.testing.typeCheckErrors(code)
     //println(s"errors: $errors")
     assert(!compiletime.testing.typeChecks(code))
  }

  /*
    // should not compiled if uncomment
  @Test def testDisableAutomaticColoringTypeCheck(): Unit = {
    @implicitAmbiguous("automatic coloring is disabled")
    implicit val dummy1: AutomaticColoringTag[ComputationBound] = new AutomaticColoringTag[ComputationBound] {}
    implicit val dummy2: AutomaticColoringTag[ComputationBound] = new AutomaticColoringTag[ComputationBound] {}
    implicit val printCode = cps.macroFlags.PrintCode
    implicit val debugLevel = cps.macroFlags.DebugLevel(20)
    val c = async {
         val x = T1.cbi(1)
         passInt(x)
    }
  }
  */

