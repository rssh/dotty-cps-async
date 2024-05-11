package cc

import org.junit.{Ignore, Test}


class Test22cc {

  @Test
  @Ignore  // yet not working
  def testCompileContextExtractor() = {

    val inDir = "testdata/set22cc/m1"
    val outDir = "testdata/set22cc/m1-out"
    val jsLinkOut = "testdata/set22cc/m1-linkout"

    val dotcInvocations = new DotcInvocations(silent=false, scalaJs = true)

    val reporter = dotcInvocations.compileFilesInDirs(List(inDir), outDir, checkAll = true)

    if (reporter.hasErrors) {
     println(reporter.summary)
    }

    assert(!reporter.hasErrors, "compilation failed")

  }

}
