package cpstest.stacktraceReporting

import scala.util.*
import cps.*
import org.junit.Test

class TestBasicStacktraceReporting {


  @Test
  def basicStackTrace() = {
    val startPos = SourcePos.current
    val c = async[ComputationBound]{
      val x = await(T1.cbi(1))
      val y = await(T1.cbi(2))
      throw new RuntimeException("test")
      x+y
    }

    val r = c.run()
    assert(r.isFailure)
    r match
      case  Failure(ex) =>
        //ex.printStackTrace()
        //println(s"startPos = ${startPos}")
        assert(ex.getStackTrace().exists(_.getLineNumber() == startPos.line + 4))
      case  Success(e) =>
        assert(false)
  }

  @Test
  def testStackTraceFromHOFunArg() = {
    val startPos = SourcePos.current
    val c = async[ComputationBound] {
      val x = List(1,2,3).map{ x =>
         if (x%2) == 1 then throw new RuntimeException("test") else x + await(T1.cbi(1))
      }
      x
    }
    val r = c.run()
    r match
      case Failure(ex) =>
        //ex.printStackTrace()
        //println(s"startPos = ${startPos}")
        assert(ex.getStackTrace().exists(_.getLineNumber() == startPos.line + 3))
      case Success(e) =>
        assert(false)

  }

}
