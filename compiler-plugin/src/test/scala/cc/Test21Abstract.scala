package cc

import org.junit.Test

class Test21Abstract {

    @Test
    def testAbstractJVM(): Unit = {
      DotcInvocations.compileAndRunFilesInDirAndCheckResult(
        "testdata/set21abstract/m1",
        "m1.Main",
        //DotcInvocationArgs(extraDotcArgs = List("-Vprint:rssh.cps"))
      )
    }


}
