package cc

import org.junit.Test

/**
 * Some test-cases from big test-case, running and minimized separately.
 */
class Test13TestCases {

  def compileCommon(): Unit = {
    if (!Test13TestCases.commonCompiled) {
      DotcInvocations.compileFilesInDir("testdata/set13TestCases/common")
      Test13TestCases.commonCompiled = true
      println("-----finish common compilation-----")
    }
  }

  def compileAfterCommon(dirname: String): Unit = {
    compileCommon()
    val classpath = s"testdata/set13TestCases/common:${System.getProperty("java.class.path")}"
    val secondInvokationArgs = DotcInvocations.InvocationArgs(extraDotcArgs = List("-classpath", classpath))
    DotcInvocations.compileFilesInDir(dirname, secondInvokationArgs)
  }

  @Test
  def testCompileBaseFutureTest(): Unit = {
    DotcInvocations.compileFilesInDir("testdata/set13TestCases/m1")
  }


  @Test
  def testCompileAsyncListStress(): Unit = {
    DotcInvocations.compileFilesInDir("testdata/set13TestCases/m2")
  }


  @Test
  def testCompileCBS1ApplyMin(): Unit = {
    compileAfterCommon("testdata/set13TestCases/m3")
  }

  @Test
  def testCompileCBS1ApplyMin2(): Unit = {
    compileAfterCommon("testdata/set13TestCases/m3_min2")
  }

  @Test
  def testCompileCBS1ApplyMin3(): Unit = {
    compileAfterCommon("testdata/set13TestCases/m3_min3")
  }


  @Test
  def testCompileCBS1DShiftedFunctionM4min1(): Unit = {
    compileAfterCommon("testdata/set13TestCases/m4_min1")
  }


  @Test
  def testCompileCBS1DShiftedFunctionM4min2(): Unit = {
    compileAfterCommon("testdata/set13TestCases/m4_min2")
  }



  @Test
  def testCommpileCBS1ShiftTryMonadM5min1(): Unit = {
    compileAfterCommon("testdata/set13TestCases/m5_min1")
  }


  @Test
  def testCompileCBS1ShiftWithFilterM6min1(): Unit = {
    compileAfterCommon("testdata/set13TestCases/m6_min1")
  }


  @Test
  def testFutureRangeWithFilterShiftM7min1(): Unit = {
    compileAfterCommon("testdata/set13TestCases/m7_min1")
  }


  @Test
  def test59M8min1(): Unit = {
    compileAfterCommon("testdata/set13TestCases/m8_min1")
  }


  @Test
  def testCBBooleanOpShortCircuits(): Unit = {
    compileAfterCommon("testdata/set13TestCases/m9_m1")
  }



  @Test
  def testCollectionMonads(): Unit = {
    compileAfterCommon("testdata/set13TestCases/m10_m1")
  }


   @Test
   def testFM2(): Unit = {
      compileAfterCommon("testdata/set13TestCases/m11_m1")
   }

  @Test
  def testUtestLike(): Unit = {
    compileAfterCommon("testdata/set13TestCases/m12_m1")
  }


}


object Test13TestCases {

  // Hack
  var commonCompiled = false

}