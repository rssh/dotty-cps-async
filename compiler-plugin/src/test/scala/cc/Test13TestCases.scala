package cc

import org.junit.Test

/**
 * Some test-cases from big test-case, running and minimized separately.
 */
class Test13TestCases {


  @Test
  def testCompileBaseFutureTest(): Unit = {
    DotcInvocations.compileFilesInDir("testdata/set13TestCases/m1")
  }

  @Test
  def testCompileAsyncListStress(): Unit = {
    DotcInvocations.compileFilesInDir("testdata/set13TestCases/m2")
  }


  def compileCommon(): Unit = {
    if (!Test13TestCases.commonCompiled) {
      DotcInvocations.compileFilesInDir("testdata/set13TestCases/common")
      Test13TestCases.commonCompiled = true
      println("-----finish common compilation-----")
    }
  }


  @Test
  def testCompileCBS1Apply(): Unit = {
    compileCommon()
    val classpath = s"testdata/set13TestCases/common:${System.getProperty("java.class.path")}"
    val secondInvokationArgs = DotcInvocations.InvocationArgs(extraDotcArgs = List("-classpath", classpath))
    DotcInvocations.compileFilesInDir("testdata/set13TestCases/m3", secondInvokationArgs)
  }

  @Test
  def testCompileCBS1ApplyMin(): Unit = {
    compileCommon()
    val classpath = s"testdata/set13TestCases/common:${System.getProperty("java.class.path")}"
    val secondInvokationArgs = DotcInvocations.InvocationArgs(extraDotcArgs = List("-classpath", classpath))
  }

  @Test
  def testCompileCBS1ApplyMin2(): Unit = {
    compileCommon()
    val classpath = s"testdata/set13TestCases/common:${System.getProperty("java.class.path")}"
    val secondInvokationArgs = DotcInvocations.InvocationArgs(extraDotcArgs = List("-classpath", classpath))
    DotcInvocations.compileFilesInDir("testdata/set13TestCases/m3_min2", secondInvokationArgs)
  }

  @Test
  def testCompileCBS1ApplyMin3(): Unit = {
    compileCommon()
    val classpath = s"testdata/set13TestCases/common:${System.getProperty("java.class.path")}"
    val secondInvokationArgs = DotcInvocations.InvocationArgs(extraDotcArgs = List("-classpath", classpath))
    DotcInvocations.compileFilesInDir("testdata/set13TestCases/m3_min3", secondInvokationArgs)
  }


  @Test
  def testCompileCBS1DShiftedFunctionM4min1(): Unit = {
    compileCommon()
    val classpath = s"testdata/set13TestCases/common:${System.getProperty("java.class.path")}"
    val secondInvokationArgs = DotcInvocations.InvocationArgs(extraDotcArgs = List("-classpath", classpath))
    DotcInvocations.compileFilesInDir("testdata/set13TestCases/m4_min1", secondInvokationArgs)
  }


}


object Test13TestCases {

  // Hack
  var commonCompiled = false

}