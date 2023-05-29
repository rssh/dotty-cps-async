package cc

import org.junit.Test


class Test10FunNoArgs {


  @Test
  def testCompileAndRunM1(): Unit = {
    DotcInvocations.compileAndRunFilesInDirAndCheckResult(
      "testdata/set10FunNoArgs/m1",
      "cpstest.Test10m1"
    )
  }


  @Test
  def testCompileAndRunM2(): Unit = {
    DotcInvocations.compileAndRunFilesInDirAndCheckResult(
      "testdata/set10FunNoArgs/m2",
      "cpstest.Test10m2"
    )
  }

}
