package cc

import org.junit.Test

class Test9Try {

  @Test
  def testCompileAndRunM1(): Unit = {
    val dotcInvocations = new DotcInvocations()

    val (code, output) = dotcInvocations.compileAndRunFilesInDir(
      "testdata/set9Try/m1",
      "testdata/set9Try/m1",
      "cpstest.Test9m1"
    )

    val reporter = dotcInvocations.reporter
    println("summary: " + reporter.summary)

    assert(reporter.allErrors.isEmpty, "There should be no errors")

    println(s"output=${output}")
    assert(output == "Right(10)\n", "The output should be `Right(10)\\n`")

  }


}
