package cps

import org.junit.{Test,Ignore}
import org.junit.Assert._

import scala.quoted._
import scala.util.Success

class TestPatternBlockMismatch:

  @Test def tValDef(): Unit = 
     val c = Async.async[ComputationBound]{
              { val t = 3 }
              val t = 4 
             }
     Console.println(s"!!!!: c==$c")
     val c1 = c.run()
     Console.println(s"!!!!: c1==$c1")
     assert(c1 == Success(()))


