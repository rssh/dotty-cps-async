package cc

import org.junit.Test

class Test12 {

  /*
  @Test
  def testCompileAndRunM1(): Unit =
    val dotcInvocations = new DotcInvocations()
    DotcInvocations.compileAndRunFilesInDirAndCheckResult(
      "testdata/set12/m1",
      "cpstest.Test12m1",
      "myurltransformed\n"
    )
    val reporter        = dotcInvocations.reporter
    println("summary: " + reporter.summary)
   */

  @Test
  def testCompileAndRunM2(): Unit =
    DotcInvocations.checkRuns(selection = (".*".r))(
      TestRun("testdata/set12/m2", "cpstest.Test12m2", "prefixmyurltransformed\n"),
      //TestRun("testdata/set12/m3", "cpstest.Test12m3", "prefixmyurltransformed/item\n")
    )

}
