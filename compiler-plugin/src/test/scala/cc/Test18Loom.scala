package cc

import org.junit.Test
import cc.DotcInvocations.IsAlreadyCompiledFlag

class Test18Loom {

  def compileAndRunTestAfterCommon(dirname: String, testClassName: String, invocationArgs: DotcInvocationArgs = DotcInvocationArgs()): Unit = {
    DotcInvocations.compileAndRunJunitTestAfterDependency(dirname, testClassName, invocationArgs, Test18Loom.commonDependency)
  }


  @Test
  def testHOFunctionArg() = {
    compileAndRunTestAfterCommon("testdata/set18loom/m1", "cpsloomtest.TestHOFunctionArgLoomTransform",
      DotcInvocationArgs(checkAll = true,
        extraDotcArgs = List(
          // "-Vprint:rssh.cps"
        )
      )
    )
  }


  @Test
  def testTwiceWithFuture() = {
    compileAndRunTestAfterCommon("testdata/set18loom/m2", "cpsloomtest.TestTwice",
      DotcInvocationArgs(checkAll = true,
        extraDotcArgs = List(
          // "-Vprint:rssh.cps"
        )
      )
    )
  }



  @Test
  def testPME() = {
    compileAndRunTestAfterCommon("testdata/set18loom/m3", "cpsloomtest.TestPME",
      DotcInvocationArgs(checkAll = true,
        extraDotcArgs = List(
          "-Vprint:rssh.cps"
        )
      )
    )
  }


  @Test
  def testIncrMin() = {
    compileAndRunTestAfterCommon("testdata/set18loom/m4", "cpsloomtest.TestIncrMin",
      DotcInvocationArgs(checkAll = true,
        extraDotcArgs = List(
         // "-Vprint:rssh.cps"
        )
      )
    )
  }


}

object Test18Loom {

  object commonCompiled extends IsAlreadyCompiledFlag {
    var isAlreadyCompiled  = false
  }
  val commonDependency = DotcInvocations.Dependency("testdata/set18loom/common", commonCompiled)


}