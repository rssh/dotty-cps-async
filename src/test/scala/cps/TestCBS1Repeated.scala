package cps

import org.junit.{Test,Ignore}
import org.junit.Assert._

import scala.quoted._
import scala.util.Success



class TestCBS1Repeated


  //@Test def repeated_0(): Unit = 
  //   val c = async[ComputationBound]{
  //     val (b,c) = (2,4)
  //     s"a ${b} ${c}"
  //   }
  //   assert(c.run() == Success("a 2 4"))

  @Test def repeated_0c(): Unit = 
     val c = async[ComputationBound]{
       f("x",1,2,3,4)
     }
     assert(c.run() == Success("a 2 4"))

  def f(x:String, y: Int*):String =
      x + y.mkString


