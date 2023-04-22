package cc

import org.junit.Test

class TestCompileExample1 {

  @Test
  def testCompileExample1(): Unit = {
    val dotcInvocations = new DotcInvocations()

    val reporter = dotcInvocations.compileFilesInDir(
      "testdata/set1/src/cpstest",
      "testdata/set1/target")

    println("summary: " + reporter.summary)

    assert( reporter.allErrors.isEmpty, "There should be no errors" )

  }

}
