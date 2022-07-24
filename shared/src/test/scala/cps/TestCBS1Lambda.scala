package cps

import org.junit.{Test,Ignore}
import org.junit.Assert._

import scala.quoted._
import scala.util.Success

import cps.testconfig.given

trait P {
  type N
  def x: Int
}

class PINT(v:Int) extends P {
   type N = Int
   def x = v
}

class TestCBS1Lambda:


  // dotty bug: https://github.com/lampepfl/dotty/issues/8146
  //@Test def lambda_depend0(): Unit = 
  //   val c = async[ComputationBound]{
  //      ((x:P, y:x.N) => s"${x.x}:${y}" )(PINT(3),2)
  //   }
  //   assert(c.run() == Success("3:2"))


  @Test def lambda_min(): Unit = 
     val c = async[ComputationBound]{
       ((x:Int) => x.toString)(3)
     }
     assert(c.run() == Success("3"))


