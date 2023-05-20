package cc

import org.junit.Test

class Test6Throw {

  @Test
  def testCompileAndRunThrow_m1(): Unit = {
    val dotcInvocations = new DotcInvocations()

    val (code, output) = dotcInvocations.compileAndRunFilesInDir(
      "testdata/set6Throw/m1",
      "testdata/set6Throw/m1/target",
      "cpstest.Test6m1"
    )

    val reporter = dotcInvocations.reporter
    println("summary: " + reporter.summary)

    assert(reporter.allErrors.isEmpty, "There should be no errors")

    println(s"output=${output}")
    assert(output == "Error during fetch url2\n", "The output should be `Error during fetch url2\\n`")

  }


}
