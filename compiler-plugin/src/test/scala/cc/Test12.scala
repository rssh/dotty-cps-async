package cc

import cps.plugin.*

import org.junit.{ Ignore, Test }

class Test12 {

  @Ignore
  @Test
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
  def testCompileAndRunM2M4(): Unit =
    DotcInvocations.checkRuns(selection = (".*".r))(
      // TestRun("testdata/set12/m2", "cpstest.Test12m2", "prefixmyurltransformed\n")
      TestRun(
        "testdata/set12/m4",
        "cpstest.Test12m4",
        "{myurl1,myurl2,myurl3}transformed\n",
        extraDotcArgs = List("-Vprint:rssh.cps")
      )
    )

  @Ignore
  @Test
  def testCompileAndRunM(): Unit =
    DotcInvocations.checkRuns(selection = (".*".r))(
      TestRun("testdata/set12/m", "cpstest.Test12m", "Hello world\n")
    )

  @Ignore
  @Test
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
