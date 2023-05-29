package cc

import org.junit.Test

class Test8WhileDo {

  @Test
  def testCompileAndRunWhileDom1(): Unit = {
      DotcInvocations.compileAndRunFilesInDirAndCheckResult(
        "testdata/set8WhileDo/m1",
        "cpstest.Test8m1",
        "myurl:3\n"
      )
  }


  @Test
  def testCompileAndRunWhileDom2(): Unit = {
      DotcInvocations.compileAndRunFilesInDirAndCheckResult(
        "testdata/set8WhileDo/m2",
        "cpstest.Test8m2",
        "myurl:1\nmyurl:2\nmyurl:3\nOk\n"
      )
  }


  @Test
  def testCompileAndRunWhileDom3(): Unit = {
    DotcInvocations.compileAndRunFilesInDirAndCheckResult(
        "testdata/set8WhileDo/m3",
        "cpstest.Test8m3",
        "myurl:1\nmyurl:2\nmyurl:3\nOk\n"
    )
  }

  // async condition + async body
  @Test
  def testCompileAndRunWhileDom4(): Unit = {
    DotcInvocations.compileAndRunFilesInDirAndCheckResult(
      "testdata/set8WhileDo/m4",
      "cpstest.Test8m4",
      "myurl:1\nmyurl:2\nmyurl:3\n()\n"
    )
  }

}
