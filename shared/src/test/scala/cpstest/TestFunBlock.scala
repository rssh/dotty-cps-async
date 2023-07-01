package cpstest

import cps.*
import cps.testconfig.given

import scala.util.Success

import org.junit.{Test,Ignore}

class TestFunBlock {

  @Test
  def testApplyWithFunBlock(): Unit = {
      var x = 0
      val c = async[ComputationBound] {
          val a = 1
          { x=1; a} + await(T1.cbi(2))
      }
      assert(c.run() == Success(3))
  }

}
