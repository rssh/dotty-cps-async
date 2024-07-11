package cc

import org.junit.{Ignore, Test}


class Test22cc {

  @Test
  @Ignore
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
  @Ignore  //  shpuld be disabled for scala-3.3.x
    // (needs -experimental option which is available starting from 3.4.0)
  def testCompileJSAsyncWithInternalCpsAsync() = {
    
    val inDir = "testdata/set22cc/m2"
    val outDir = "testdata/set22cc/m2-out"
    val jsLinkOut = "testdata/set22cc/m2-linkout"

    val dotcInvocations = new DotcInvocations(silent = false, scalaJs = true)

    // checkAll disabled due to https://github.com/scala/scala3/issues/21119
    val reporter = dotcInvocations.compileFilesInDirs(List(inDir), outDir, checkAll = false,
      extraArgs = List( //"-Vprint:erasure,rssh.cps", 
                        "-experimental", 
                        // "-P:rssh.cps:printCode"
                       ),
      usePlugin = true
    )

    if (reporter.hasErrors) {
      println("errors:")
      for(error <- reporter.allErrors)
        val pos = error.pos
        println(s"${pos.source}:${pos.startLine}:${pos.startColumn} ${error.msg}")
      println(reporter.summary)
    }

    assert(!reporter.hasErrors, "compilation failed")

  }

  @Test
  @Ignore  // solved, not in focus.
  def testCompileJSResolveReject() = {

    val inDir = "testdata/set22cc/m3"
    val outDir = "testdata/set22cc/m3-out"
    val jsLinkOut = "testdata/set22cc/m3-linkout"

    val dotcInvocations = new DotcInvocations(silent = false, scalaJs = true)

    val reporter = dotcInvocations.compileFilesInDirs(List(inDir), outDir, checkAll = true,
      extraArgs = List("-Vprint:erasure,rssh.cps", "-Yprint-syms", "-experimental" ),
      usePlugin = false
    )

    if (reporter.hasErrors) {
      println(reporter.summary)
    }

    assert(!reporter.hasErrors, "compilation failed")

  }
}
