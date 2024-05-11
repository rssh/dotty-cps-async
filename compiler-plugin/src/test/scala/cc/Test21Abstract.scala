package cc

import org.junit.{Test}

class Test21Abstract {


    @Test
    def testAbstractContextParamJVM(): Unit = {
      DotcInvocations.compileAndRunFilesInDirAndCheckResult(
        "testdata/set21abstract/m1",
        "m1.Main",
        //DotcInvocationArgs(extraDotcArgs = List("-Vprint:rssh.cps"))
      )
    }



    @Test
    def testAbstractRetLambdaJVM(): Unit = {
      DotcInvocations.compileAndRunFilesInDirAndCheckResult(
        "testdata/set21abstract/m2",
        "m2.Main",
        invocationArgs = DotcInvocationArgs(silent=false, extraDotcArgs = List("-Vprint:erasure,rssh.cps"))
      )
    }




}
