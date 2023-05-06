package cc

import org.junit.Test

class TestExample1 {

  @Test
  def testCompileExample1(): Unit = {
    val dotcInvocations = new DotcInvocations()

    val reporter = dotcInvocations.compileFilesInDir(
      "testdata/set1/src/cpstest",
      "testdata/set1/target")

    println("summary: " + reporter.summary)

    assert( reporter.allErrors.isEmpty, "There should be no errors" )

  }

  @Test
  def testCompileAndRunExample1(): Unit = {
    val dotcInvocations = new DotcInvocations()

    val (code,output0) = dotcInvocations.compileAndRunFilesInDirs(
      List("testdata/set1/src/cpstest"),
      "testdata/set1/target",
      "cpstest.TestExample1"
    )

    val output = output0.trim
    println("summary: " + dotcInvocations.reporter.summary)

    assert(dotcInvocations.reporter.allErrors.isEmpty, "There should be no errors")
    println(s"output=${output}")
    assert(output.trim == "Ok", s"Output should be 'Ok', we have '${output}'")

  }


}
