package cc

import org.junit.Test

class Test4m1 {

  @Test
  def testCompileExample4m1(): Unit = {
    val dotcInvocations = new DotcInvocations()

    val reporter = dotcInvocations.compileFilesInDir(
      "testdata/set4/m1",
      "testdata/set4/m1/target")

    println("summary: " + reporter.summary)

    assert(reporter.allErrors.isEmpty, "There should be no errors")

  }

}
