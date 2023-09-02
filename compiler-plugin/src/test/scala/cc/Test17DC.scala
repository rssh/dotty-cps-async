package cc

import org.junit.Test

class Test17DC {

  @Test
  def testCompileAndDCIn(): Unit = {
    DotcInvocations.compileAndRunFilesInDirAndCheckResult(
      "testdata/set17/m1",
      "cpstest.TestShiftedDirectM1",
      "Ok\n",
      invocationArgs = DotcInvocations.InvocationArgs(extraDotcArgs = List("-Vprint:rssh.cps")),
    )
  }


}
