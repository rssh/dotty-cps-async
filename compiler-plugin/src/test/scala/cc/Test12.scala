package cc

import org.junit.Test

class Test12 {

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

}
