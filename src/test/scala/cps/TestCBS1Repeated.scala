package cps

import org.junit.{Test,Ignore}
import org.junit.Assert._

import scala.quoted._
import scala.util.Success



class TestCBS1Repeated:

  def qqq:Int = 0
  
/*
  @Test def repeated_0(): Unit = 
     val c = async[ComputationBound]{
       val (b,c) = (2,4)
       s"a ${b} ${c}"
     }
     assert(c.run() == Success("a 2 4"))

  @Test def repeated_0c(): Unit = 
     val c = async[ComputationBound]{
       f("x",1,2,3,4)
     }
     assert(c.run() == Success("a 2 4"))

  @Test def repeated_0a(): Unit = 
     val c = async[ComputationBound]{
       val s = Seq(12,13,14)
       f("x",s :_* )
     }

  def f(x:String, y: Int*):String =
      x + y.mkString

*/

