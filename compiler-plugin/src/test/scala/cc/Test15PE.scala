package cc

import cc.DotcInvocations.IsAlreadyCompiledFlag
import org.junit.Test
import org.junit.Assert.*

import scala.concurrent.*
import scala.concurrent.duration.*

class Test15PE {


  def compileAndRunTestAfterCommon(dirname: String, testClassName: String, invocationArgs: DotcInvocationArgs = DotcInvocationArgs()): Unit = {
    DotcInvocations.compileAndRunJunitTestAfterDependency(dirname, testClassName, invocationArgs, Test15PE.dependency)
  }


  @Test
  def testFizzBuzz() = {
    compileAndRunTestAfterCommon("testdata/set15pe/m1", "cps.pe.TestFizzBuzz",
      DotcInvocationArgs(checkAll = true,
        extraDotcArgs = List(
         // "-Vprint:rssh.cps"
        )
      )
    )
  }


  @Test
  def testFizzBuzzLocalDirectRef() = {
    compileAndRunTestAfterCommon("testdata/set15pe/m2", "cps.pe.TestFizzBuzz",
      DotcInvocationArgs(checkAll = true,
        extraDotcArgs = List(
          // "-Vprint:rssh.cps", "-Vprint:erasure"
        )
      )
    )
  }


  @Test
  def testUseResourceM3() = {
    compileAndRunTestAfterCommon("testdata/set15pe/m3", "cps.pe.TestUseResource",
      DotcInvocationArgs(checkAll = true,
        extraDotcArgs = List(
         // "-Vprint:rssh.cps", "-Vprint:erasure"
        )
      )
    )
  }


  @Test
  def testUseResourceM4() = {
    compileAndRunTestAfterCommon("testdata/set15pe/m4", "cps.pe.TestUseResourceFromPlugin",
      DotcInvocationArgs(checkAll = true,
        extraDotcArgs = List(
         //  "-Vprint:rssh.cps", "-Vprint:erasure"
        )
      )
    )
  }



  @Test
  def testFizzBuzzWithInlineDirectRefM5() = {
    compileAndRunTestAfterCommon("testdata/set15pe/m5", "cps.pe.TestFizzBuzzM5",
      DotcInvocationArgs(checkAll = false,
        extraDotcArgs = List(
          //"-Vprint:rssh.cps", "-Vprint:erasure"
        )
      )
    )
  }


  @Test
  def testFizzBuzzWithInlineDirectRefM6() = {
    compileAndRunTestAfterCommon("testdata/set15pe/m6", "cps.pe.TestFizzBuzzM6",
      DotcInvocationArgs(checkAll = false,
        extraDotcArgs = List(
          //"-Vprint:rssh.cps", "-Vprint:erasure"
        )
      )
    )
  }



  @Test
  def testFizzBuzzWithInlineDirectRefM7() = {
    compileAndRunTestAfterCommon("testdata/set15pe/m7", "cps.pe.TestFizzBuzzM7",
      DotcInvocationArgs(checkAll = false,
        extraDotcArgs = List(
          //"-Vprint:rssh.cps", "-Vprint:erasure"
        )
      )
    )
  }


  @Test
  def testInlineOpInResourceM8() = {
    compileAndRunTestAfterCommon("testdata/set15pe/m8", "cps.pe.TestInlineOpInResourceM8",
      DotcInvocationArgs(checkAll = true,
        extraDotcArgs = List(
         // "-Vprint:rssh.cps", "-Vprint:erasure"
        )
      )
    )
  }



}

object Test15PE  {

  object commonCompiled extends IsAlreadyCompiledFlag{
    var isAlreadyCompiled: Boolean = false
  }
  def dependency = DotcInvocations.Dependency("testdata/set15pe/common",  commonCompiled)

}
