package cc

import org.junit.Test

class Test7Import {


  @Test
  def testCompileAndRunImpoer(): Unit = {
    val dotcInvocations = new DotcInvocations()

    val (code, output) = dotcInvocations.compileAndRunFilesInDirJVM(
      "testdata/Set7Import/src",
      "testdata/Set7Import/classes",
      "cpstest.Test7"
    )

    val reporter = dotcInvocations.reporter
    println("summary: " + reporter.summary)

    assert(reporter.allErrors.isEmpty, "There should be no errors")

    println(s"output=${output}")
    assert(output == "2\n", "The output should be `2\\n`")

  }


}
