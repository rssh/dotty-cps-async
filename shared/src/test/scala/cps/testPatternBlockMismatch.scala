package cps

import org.junit.{Test,Ignore}
import org.junit.Assert._

import scala.quoted._
import scala.util.Success

class TestPatternBlockMismatch:

  @Test def tValDef(): Unit = 
     val c = async[ComputationBound]{
              { val t = 3 }
              val t = 4 
             }
     val c1 = c.run()
     assert(c1 == Success(()))


