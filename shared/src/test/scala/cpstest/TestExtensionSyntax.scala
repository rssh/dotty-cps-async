package cpstest



import cps.*
import org.junit.Test

import scala.util.Success

class TestExtensionSyntax {

  @Test
  def testCBExtensionSyntax() = {

    val c = async[ComputationBound] {
      val a = T1.cbi(1)
      val b = T1.cbi(2)
      a.await + b.await
    }
    assert(c.run() == Success(3))

  }

  @Test
  def testCBReflectExtensionSyntax() = {

    val c = reify[ComputationBound] {
      val a = T1.cbi(1)
      val b = T1.cbi(2)
      a.reflect + b.reflect
    }
    assert(c.run() == Success(3))

  }


}
