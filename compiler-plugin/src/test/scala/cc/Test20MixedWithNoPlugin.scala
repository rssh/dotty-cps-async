package cc

import org.junit.Test

class Test20MixedWithNoPlugin {


  @Test
  def test20M1(): Unit = {
    val m1Out = "testdata/set20withNoPlugin/out-m1"
    val currentClasspath = System.getProperty("java.class.path")
    val m1InvocationArgsCmLib = DotcInvocationArgs(outDir = Some(m1Out))
    val classpath1 = s"${m1Out}:${currentClasspath}"
    val secondInvokationArgs = m1InvocationArgsCmLib.copy(extraDotcArgs = List("-classpath", classpath1) ++ m1InvocationArgsCmLib.extraDotcArgs)
    DotcInvocations.succesfullyCompileFilesInDir("testdata/set20withNoPlugin/cm-lib", m1InvocationArgsCmLib)
    DotcInvocations.succesfullyCompileFilesInDir("testdata/set20withNoPlugin/valid-call", secondInvokationArgs)
    val mainClass = "vc.Main"
    val (code, out) = DotcInvocations.runJVMInClasspath(mainClass, classpath1)
    assert(code == 0)
    //println(s"code=$code, out=$out")
  }

  @Test
  def test20M2(): Unit = {
    val m2Out = "testdata/set20withNoPlugin/out-m2"
    val currentClasspath = System.getProperty("java.class.path")
    // accidently compile library without plugin
    val invocationArgsCmLib = DotcInvocationArgs(outDir = Some(m2Out), usePlugin = false)
    val classpath1 = s"${m2Out}:${currentClasspath}"
    val secondInvokationArgs = invocationArgsCmLib.copy(
      extraDotcArgs = List("-classpath", classpath1) ++ invocationArgsCmLib.extraDotcArgs,
      usePlugin = true
    )
    DotcInvocations.succesfullyCompileFilesInDir("testdata/set20withNoPlugin/cm-lib", invocationArgsCmLib)
    // should fail in compile time
    val reporter = DotcInvocations.compileFilesInDir("testdata/set20withNoPlugin/invalid-call", secondInvokationArgs)
    assert(reporter.errorCount > 0)
    if (reporter.errorCount == 0) {
      println("Warning: expected to fail in compile time")
      val mainClass = "ivc.Main"
      val (code, out) = DotcInvocations.runJVMInClasspath(mainClass, classpath1)
      assert(code == 0)
    }
  }



  @Test
  def test20M3(): Unit = {
    val m3Out = "testdata/set20withNoPlugin/out-m3"
    val currentClasspath = System.getProperty("java.class.path")
    val invocationArgsCmLib = DotcInvocationArgs(outDir = Some(m3Out))
    val classpath1 = s"${m3Out}:${currentClasspath}"

    // Now when  we forgott to use plugin in the second invocation
    val secondInvokationArgs = invocationArgsCmLib.copy(
      extraDotcArgs = List("-classpath", classpath1) ++ invocationArgsCmLib.extraDotcArgs ++ List( "-Vprint:erasure"),
      usePlugin = false
    )
    DotcInvocations.succesfullyCompileFilesInDir("testdata/set20withNoPlugin/cm-lib", invocationArgsCmLib)
    val reporter = DotcInvocations.compileFilesInDir("testdata/set20withNoPlugin/invalid-call", secondInvokationArgs)
    if (reporter.errorCount == 0) {
      println("Warning: expected to fail in compile time")
      val mainClass = "ivc.Main"
      val (code, out) = DotcInvocations.runJVMInClasspath(mainClass, classpath1)
      assert(code == 0)
    }
  }

  @Test
  def test20M4(): Unit = {
    // both invocations without plugin
    val m4Out = "testdata/set20withNoPlugin/out-m4"
    val currentClasspath = System.getProperty("java.class.path")
    val invocationArgsCmLib = DotcInvocationArgs(outDir = Some(m4Out), usePlugin = false)
    val classpath1 = s"${m4Out}:${currentClasspath}"
    val secondInvokationArgs = invocationArgsCmLib.copy(
      extraDotcArgs = List("-classpath", classpath1) ++ invocationArgsCmLib.extraDotcArgs ++ List( "-Vprint:erasure"),
      usePlugin = false
    )
    DotcInvocations.succesfullyCompileFilesInDir("testdata/set20withNoPlugin/cm-lib", invocationArgsCmLib)
    val reporter = DotcInvocations.compileFilesInDir("testdata/set20withNoPlugin/invalid-call", secondInvokationArgs)
    assert(reporter.errorCount > 0)
    if (reporter.errorCount == 0) {
      println("Warning: expected to fail in compile time")
      val mainClass = "ivc.Main"
      val (code, out) = DotcInvocations.runJVMInClasspath(mainClass, classpath1)
      assert(code == 0)
    }
  }



  @Test
  def test20M5(): Unit = {
    // cm-lib without plugin, local-direct-call with plugin
    val m5Out = "testdata/set20withNoPlugin/out-m5"
    val currentClasspath = System.getProperty("java.class.path")
    val invocationArgsCmLib = DotcInvocationArgs(outDir = Some(m5Out), usePlugin = false)
    val classpath1 = s"${m5Out}:${currentClasspath}"
    val secondInvokationArgs = invocationArgsCmLib.copy(
      extraDotcArgs = List("-classpath", classpath1) ++ invocationArgsCmLib.extraDotcArgs,
      usePlugin = true
    )
    DotcInvocations.succesfullyCompileFilesInDir("testdata/set20withNoPlugin/cm-lib", invocationArgsCmLib)
    val reporter = DotcInvocations.compileFilesInDir("testdata/set20withNoPlugin/local-direct-call", secondInvokationArgs)
    //assert(reporter.errorCount > 0)
    if (reporter.errorCount == 0) {
      println("Warning: expected to fail in compile time")
      val mainClass = "vc.Main"
      val (code, out) = DotcInvocations.runJVMInClasspath(mainClass, classpath1)
      assert(code == 0)
    }
  }

  @Test
  def test20M6(): Unit = {
    // cm-lib with plugin, local-direct-call without plugin
    val m6Out = "testdata/set20withNoPlugin/out-m6"
    val currentClasspath = System.getProperty("java.class.path")
    val invocationArgsCmLib = DotcInvocationArgs(outDir = Some(m6Out))
    val classpath1 = s"${m6Out}:${currentClasspath}"
    val secondInvokationArgs = invocationArgsCmLib.copy(
      extraDotcArgs = List("-classpath", classpath1) ++ invocationArgsCmLib.extraDotcArgs,
      usePlugin = false
    )
    DotcInvocations.succesfullyCompileFilesInDir("testdata/set20withNoPlugin/cm-lib", invocationArgsCmLib)
    val reporter = DotcInvocations.compileFilesInDir("testdata/set20withNoPlugin/local-direct-call", secondInvokationArgs)
    assert(reporter.errorCount > 0)
    if (reporter.errorCount == 0) {
      println("Warning: expected to fail in compile time")
      val mainClass = "vc.Main"
      val (code, out) = DotcInvocations.runJVMInClasspath(mainClass, classpath1)
      assert(code == 0)
    }
  }


}
