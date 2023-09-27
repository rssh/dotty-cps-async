package cc

import org.junit.Test

class Test17DC {

  @Test
  def testCompileAndDCIn(): Unit = {
    DotcInvocations.compileAndRunFilesInDirAndCheckResult(
      "testdata/set17/m1",
      "cpstest.TestShiftedDirectM1",
      "Ok\n",
      invocationArgs = DotcInvocationArgs(extraDotcArgs = List("-Vprint:rssh.cps")),
    )
  }


}
