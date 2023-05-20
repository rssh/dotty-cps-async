package cc

import org.junit.Test

class Test5Assign {


  @Test
  def testCompileAndRunSimpleAssign_m1(): Unit = {
    val dotcInvocations = new DotcInvocations()

    val (code, output) = dotcInvocations.compileAndRunFilesInDir(
      "testdata/set5Assign/m1",
      "testdata/set5Assign/m1/target",
      "cpstest.Test5m1"
    )

    val reporter = dotcInvocations.reporter
    println("summary: " + reporter.summary)

    assert(reporter.allErrors.isEmpty, "There should be no errors")

    println(s"output=${output}")
    assert(output == "result:ok:myurl\n", "The output should be `result:ok:myurl`")

  }


  @Test
  def testSelectAssyncAssign_m2(): Unit = {
    val dotcInvocations = new DotcInvocations()

    val (code, output) = dotcInvocations.compileAndRunFilesInDir(
      "testdata/set5Assign/m2",
      "testdata/set5Assign/m2/target",
      "cpstest.Test5m2"
    )

    val reporter = dotcInvocations.reporter
    println("summary: " + reporter.summary)

    assert(reporter.allErrors.isEmpty, "There should be no errors")

    println(s"output=${output}")
    assert(output == "ok:myurl\n", "The output should be ok:myurl`")

  }


}
