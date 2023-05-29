package cc

import org.junit.Test

class Test9Try {


  @Test
  def testCompileAndRunM1(): Unit = {
    DotcInvocations.compileAndRunFilesInDirAndCheckResult(
      "testdata/set9Try/m1",
      "cpstest.Test9m1",
      "Right(10)\n"
    )
  }


  @Test
  def testCompileAndRunM2(): Unit = {
    DotcInvocations.compileAndRunFilesInDirAndCheckResult(
      "testdata/set9Try/m2",
      "cpstest.Test9m2"
    )
  }


}
