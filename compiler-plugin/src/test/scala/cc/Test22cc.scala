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

    val reporter = dotcInvocations.compileFilesInDirs(List(inDir), outDir, checkAll = true,
      extraArgs = List("-Vprint:erasure,rssh.cps", "-Yprint-syms"),
    )

    if (reporter.hasErrors) {
     println(reporter.summary)
    }

    assert(!reporter.hasErrors, "compilation failed")

  }

  @Test
  def testCompileJSAsyncWithInternalCpsAsync() = {
    
    val inDir = "testdata/set22cc/m2"
    val outDir = "testdata/set22cc/m2-out"
    val jsLinkOut = "testdata/set22cc/m2-linkout"

    val dotcInvocations = new DotcInvocations(silent = false, scalaJs = true)

    val reporter = dotcInvocations.compileFilesInDirs(List(inDir), outDir, checkAll = true,
      extraArgs = List("-Vprint:erasure,rssh.cps", "-Yprint-syms", "-experimental"),
      usePlugin = true
    )

    if (reporter.hasErrors) {
      println(reporter.summary)
    }

    assert(!reporter.hasErrors, "compilation failed")

  }
  
}
