package cps

import org.junit.{Test,Ignore}
import org.junit.Assert._

import scala.annotation.nowarn
//import scala.quoted._
import scala.util.Success

import cps.testconfig.given


class TestPatternBlockMismatch:

  @Test def tValDef(): Unit =
     @nowarn("msg=A pure expression does nothing in statement position")  
     val c = async[ComputationBound]{
              { val t = 3 }
              val t = 4 
             }
     val c1 = c.run()
     assert(c1 == Success(()))


