package cc

import cps.plugin.*

import org.junit.{ Ignore, Test }

class Test12 {

  val debug = false

  @Test
  def testCompileAndRunM1(): Unit = {
    val dotcInvocations = new DotcInvocations()
    val (codeOrErrors, output) =
        dotcInvocations.compileAndRunFilesInDirJVM("testdata/set12/m1",  "testdata/set12/m1",  "cpstest.Test12m1")
    val reporter       = dotcInvocations.reporter
    if (reporter.errorCount == 0) then
      println(s"output=${output}")
      assert(false, "Expected erros was not reported")
    else
      if debug then
        println("summary: " + reporter.summary)
        println(s"output=${output}")
      assert(output.contains("high order"))
  }

  
  @Test
  def testCompileAndRunM2(): Unit =
    val dotcArgs = DotcInvocationArgs(extraDotcArgs = List("-P:rssh.cps:withShiftReplace"))
    DotcInvocations.checkRuns(selection = (".*".r),dotcArgs = dotcArgs)(
      TestRun("testdata/set12/m2", "cpstest.Test12m2", "prefixmyurl\n")
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



  @Test
  def testCompileAndRunM3(): Unit =
    val dotcInvocations = new DotcInvocations()
    val (code, output) =
        dotcInvocations.compileAndRunFilesInDirJVM("testdata/set12/m3", "testdata/set12/m3", "cpstest.Test12m3")
    val reporter       = dotcInvocations.reporter
    //println("summary: " + reporter.summary)
    //println(s"output=${output}")
    assert(output.contains("myurl/1"))

}
