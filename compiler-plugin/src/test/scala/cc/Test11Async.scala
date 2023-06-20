package cc

import org.junit.Test


class Test11Async {

  @Test
  def testCompileAndRun11(): Unit = {
    DotcInvocations.checkRuns(selection = (".*".r))(
      TestRun("testdata/set11Async/m0", "cpstest.Test11m0"),
      TestRun("testdata/set11Async/m0s", "cpstest.Test11m0s"),
      TestRun("testdata/set11Async/m1", "cpstest.Test11m1"),
    )
  }


}
