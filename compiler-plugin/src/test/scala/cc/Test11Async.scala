package cc

import org.junit.Test


class Test11Async {

  @Test
  def testCompileAndRun11(): Unit = {
    DotcInvocations.checkRuns(selection = (".*None".r))(
      TestRun("testdata/set11Async/m0", "cpstest.Test11m0"),

    )
  }


}
