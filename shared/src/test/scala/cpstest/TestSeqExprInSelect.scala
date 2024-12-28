package cpstest

import cps.*
import org.junit.Test

class TestSeqExprInSelect {

  @Test
  def testSeqExprInSelectCb() = {

    val c = reify[ComputationBound] {
      val x = MacroSeqExprInSelect.initArrayExpr1(10)
      x
    }
    val r = c.run()
    assert(r.isSuccess)
    r match
      case scala.util.Success(v) => assert(v.get(0) == 10)
      case scala.util.Failure(ex) => throw ex


  }


}
