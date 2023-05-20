package cc

import org.junit.Test


//TODO: AsyncLambda in match
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



  @Test
  def testCompileAndRunFlatMappedMatch_4m3(): Unit = {
    val dotcInvocations = new DotcInvocations()

    val (code, output) = dotcInvocations.compileAndRunFilesInDir(
      "testdata/set4/m3",
      "testdata/set4/m3/target",
      "cpstest.s4.m3.Test4m3"
    )

    val reporter = dotcInvocations.reporter
    println("summary: " + reporter.summary)

    assert(reporter.allErrors.isEmpty, "There should be no errors")

    println(s"output=${output}")
    assert(output == "1\n", "The output should be 1")

  }



}
