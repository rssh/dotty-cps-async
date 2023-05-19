package cc

import org.junit.Test

class Test4Match {


  @Test
  def testCompileExample4m1(): Unit = {
    val dotcInvocations = new DotcInvocations()

    val reporter = dotcInvocations.compileFilesInDir(
      "testdata/set4/m1",
      "testdata/set4/m1/target")

    println("summary: " + reporter.summary)

    assert(reporter.allErrors.isEmpty, "There should be no errors")

  }



  @Test
  def testCompileExample4m2(): Unit = {
    val dotcInvocations = new DotcInvocations()

    val reporter = dotcInvocations.compileFilesInDir(
      "testdata/set4/m2",
      "testdata/set4/m2/target")

    println("summary: " + reporter.summary)

    assert(reporter.allErrors.isEmpty, "There should be no errors")

  }

}
