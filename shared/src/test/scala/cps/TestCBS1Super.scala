package cps

import org.junit.{Test,Ignore}
import org.junit.Assert._

import scala.quoted._
import scala.util.Success
import cps.testconfig.given


class BaseTestCbs1Super:

  def  testFun:Int = 0

class BaseTestCbs1Super2 extends BaseTestCbs1Super:

  override def  testFun:Int = 1

class TestCBS1Super extends BaseTestCbs1Super2:

  class Root { 
    def x = "Root" 
    def ax: ComputationBound[String] = T1.cbs("R")
  }

  class A extends Root { 
     override def x = "A" ; 
     def superA = super.x 
     //override def ax:ComputationBound[String] = async( await(super.ax)+"A")
     // Compiler error
     //def ax1:ComputationBound[String] = async( await(T1.cbs("A"))+"A")
     //def ax2:ComputationBound[Int] = async( await(T1.cbi(3))+4)
     def ax3 = async( "A" + await(super.ax) )
  }

/*
  trait B extends Root { 
      override def x = "B" ; 
      def superB = super.x 
      override def ax = async( "B" + await(super.ax)+"B")
  }
 
  class C extends Root with B {
      override def x = "C" ; 
      def superC = super.x
      override def ax = async( await(super.ax)+"C")
      def superVx = async( await(super[B].ax)+await(super[Root].ax)+"C")
  }
*/

  @Test def super_a_1(): Unit = {
     val c = async{
       val a = new A
       await(a.ax3)
     }
     assert(c.run() == Success("AR"))
  }


  @Test def super_ad_0(): Unit = {
     val a = new A
     val c = a.ax3
     assert(c.run() == Success("AR"))
  }

  override def testFun: Int = 2

  @Test def super_f_1(): Unit = {
     val c = async{
       testFun - super.testFun
     }
     assert(c.run() == Success(1))
  }

  
/*
  @Test def super_C1_1(): Unit = 
     val c = async{
       val c = new C()
       await(c.superVx)
     }
     assert(c.run() == Success("RAB"+"RA"+"C"))

  @Test def super1_0(): Unit = 
     val c = async[ComputationBound]{
       val c = new C
       c.superC
     }
     //assert(c.run() == Success("B"))
*/


