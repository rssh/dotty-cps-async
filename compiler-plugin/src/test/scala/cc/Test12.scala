package cc

import cps.plugin.*
import org.junit.{Ignore, Test}


class Test12 {


  @Test
  @Ignore
  def testCompileAndRunM1(): Unit =
    val dotcInvocations = new DotcInvocations()
    try {
      val (code, output) =
        dotcInvocations.compileAndRunFilesInDirJVM("testdata/set12/m1", "cpstest.Test12m1")
      val reporter       = dotcInvocations.reporter
      println("summary: " + reporter.summary)
      println(s"output=${output}")
      //TODO: check message
      assert(false , "Expected erros was not reported")
    } catch {
      case ex: CpsTransformException =>
        assert(
          ex.getMessage == "Object has to be a high-order function",
          "There should be a CpsTransformException about high-order check"
        )
    }

  @Test
  @Ignore
  // currentlu this test overload number of thr possible compiler stages (which is now 32)
  // so it is disabled.
  //  Will be enabled after merging shiftReplace stage to already used stage.
  def testCompileAndRunM2(): Unit =
    val dotcArgs = DotcInvocationArgs(extraDotcArgs = List("-P:rssh.cps:withShiftReplace"))
    DotcInvocations.checkRuns(selection = (".*".r),dotcArgs = dotcArgs)(
      TestRun("testdata/set12/m2", "cpstest.Test12m2", "prefixmyurltransformed\n")
    )

  @Test
  @Ignore
  def testCompileAndRunM3(): Unit =
    val dotcInvocations = new DotcInvocations()
    try {
      val (code, output) =
        dotcInvocations.compileAndRunFilesInDirJVM("testdata/set12/m3", "cpstest.Test12m3")
      val reporter       = dotcInvocations.reporter
      println("summary: " + reporter.summary)
      println(s"output=${output}")
      //TODO: check message
      assert(false, "Expected errors was not reported")
    } catch {
      case ex: CpsTransformException =>
        ex.printStackTrace()
        assert(
          ex.getMessage == "Unsupported type of function. The return type must not be a function",
          "There should be a CpsTransformException about unsupported type"
        )
    }

}
