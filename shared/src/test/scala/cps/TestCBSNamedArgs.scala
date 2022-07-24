package cps

import org.junit.{Test,Ignore}
import org.junit.Assert._

import scala.quoted._
import scala.util.Success

import cps.testconfig.given


class TestCBSNamedArgs:

  def plus(x:Int, y:Int) = x-y
  
  @Test def namedargs_0(): Unit = 
     val c = async[ComputationBound]{
       plus(y = 3, x = 1)
     }
     assert(c.run() == Success(-2))

  @Test def namedargs_1(): Unit = 
     val c = async[ComputationBound]{
       plus(y = 3, x = await(T1.cbi(4)) )
     }
     assert(c.run() == Success(1))


