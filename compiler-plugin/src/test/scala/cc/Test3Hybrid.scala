package cc

import org.junit.Test


class Test3Hybrid {

  @Test
  def compileAndRunSet3(): Unit = {
    val dotcInvocations = new DotcInvocations()
    val res =dotcInvocations.compileAndRunFilesInDirs(
      List("testdata/set3"),
      "testdata/set3/target",
      "cpstest.Example3"
    )

    val reporter = dotcInvocations.reporter
    println("summary: " + reporter.summary)
    println(s"res: ${res}")

    assert(reporter.allErrors.isEmpty, "There should be no errors")


  }

}
