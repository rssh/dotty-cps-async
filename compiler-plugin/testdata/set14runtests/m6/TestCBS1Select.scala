package cps

import org.junit.{Test,Ignore}
import org.junit.Assert._

import scala.quoted._
import scala.util.Success

import cps.testconfig.given

class TestCBS1Select:

  case class P(val x:Int, val y:Int)

  object PD {
     def fx(x:Int):Int = x+1
     def fy(y:Int):Int = y+2

  }

  @Test def sel_fun_0(): Unit = 
     val c = async{
       PD.fx
     }
     assert(c.run().isSuccess)
     val c2 = c.map(x => x(1))
     assert(c2.run() == Success(2))


