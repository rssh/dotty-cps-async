package cc

import org.junit.Test

class Test2Inline {

  @Test
  def testCompileAndRunExample2(): Unit = {
    val dotcInvocations = new DotcInvocations()

    val (code, output0) = dotcInvocations.compileAndRunFilesInDirs(
      List("testdata/set2_inlinefun"),
      "testdata/set2/target",
      "cpstest.Example2Inline")


    val reporter = dotcInvocations.reporter
    println("summary: " + reporter.summary)

    assert( reporter.allErrors.isEmpty, "There should be no errors" )

    val output = output0
    assert(output=="3\n3\n", s"output=${output}")
    println(s"Test2Inline: output=${output}")
  }


}
