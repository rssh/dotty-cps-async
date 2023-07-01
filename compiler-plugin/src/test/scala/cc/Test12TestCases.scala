package cc

import org.junit.Test

/**
 * Some test-cases from big test-case, running and minimized separately.
 */
class Test12TestCases {

  @Test
  def testCompileBaseFutureTest(): Unit = {
    DotcInvocations.compileFilesInDir("testdata/set12TestCases/m1")
  }

  @Test
  def testCompileAsyncListStress(): Unit = {
    DotcInvocations.compileFilesInDir("testdata/set12TestCases/m2")
  }

}
