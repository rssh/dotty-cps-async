package cc

import org.junit.Test

class Test16Neg {

  @Test
  def testNegM1(): Unit = {
    val reporter = DotcInvocations.compileFilesInDir("testdata/set16Neg/m1")
    assert(reporter.hasErrors)
    val relatedToDirect = reporter.allErrors.exists(e => e.msg.message.contains("CpsDirect"))
    assert(relatedToDirect)
  }

  @Test
  def testNegM2(): Unit = {
    val reporter = DotcInvocations.compileFilesInDir("testdata/set16Neg/m2")
    assert(reporter.hasErrors)
    val relatedToDirect = reporter.allErrors.exists(e => e.msg.message.contains("CpsDirect"))
    assert(relatedToDirect)
  }


}
