package cps

import org.junit.{Test,Ignore}
import org.junit.Assert._

import scala.quoted._
import scala.util.Success

class TestPatternBlockMismatch

  @Test def tValDef(): Unit = 
     val c = Async.async[ComputationBound]{
              { val t = 3 }
              val t = 4 
             }
     assert(c == Done(()))


