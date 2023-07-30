package cc

import cps.plugin.*

import org.junit.Test

class Test12 {

  @Test
  def testCompileAndRunM1(): Unit =
    val dotcInvocations = new DotcInvocations()
    try {
      val (code, output) =
        dotcInvocations.compileAndRunFilesInDir("testdata/set12/m1", "cpstest.Test12m1")
      val reporter       = dotcInvocations.reporter
      println("summary: " + reporter.summary)
      println(s"output=${output}")
      assert(false, "Expected exception was not thrown")
    } catch {
      case ex: CpsTransformException =>
        assert(
          ex.getMessage == "Object has to be a high-order function",
          "There should be a CpsTransformException about high-order check"
        )
    }

  @Test
  def testCompileAndRunM2(): Unit =
    DotcInvocations.checkRuns(selection = (".*".r))(
      TestRun("testdata/set12/m2", "cpstest.Test12m2", "prefixmyurltransformed\n")
    )

  @Test
  def testCompileAndRunM3(): Unit =
    val dotcInvocations = new DotcInvocations()
    try {
      val (code, output) =
        dotcInvocations.compileAndRunFilesInDir("testdata/set12/m3", "cpstest.Test12m3")
      val reporter       = dotcInvocations.reporter
      println("summary: " + reporter.summary)
      println(s"output=${output}")
      assert(false, "Expected exception was not thrown")
    } catch {
      case ex: CpsTransformException =>
        assert(
          ex.getMessage == "Unsupported type of function. The return type must not be a function",
          "There should be a CpsTransformException about unsupported type"
        )
    }

}
