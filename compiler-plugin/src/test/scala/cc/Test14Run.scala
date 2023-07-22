package cc

import org.junit.Test

import scala.concurrent.*
import scala.concurrent.duration.*

class Test14Run {

  def compileCommon(): Unit = {
    if (!Test14Run.commonCompiled) {
      DotcInvocations.compileFilesInDir("testdata/set14runtests/common")
      Test14Run.commonCompiled = true
      println("-----finish common compilation-----")
    }
  }

  def compileAndRunTestAfterCommon(dirname: String, testClassName:String): Unit = {
    compileCommon()
    val classpath1 = s"testdata/set14runtests/common:${System.getProperty("java.class.path")}"
    val secondInvokationArgs = DotcInvocations.InvocationArgs(extraDotcArgs = List("-classpath", classpath1))
    DotcInvocations.compileFilesInDir(dirname, secondInvokationArgs)
    val classpath2 = s"${dirname}:${classpath1}"
    val mainClass = "testUtil.JunitMain"
    val cmd = s"java -cp $classpath2 $mainClass $testClassName"
    println(s"Running $cmd")
    val process = Runtime.getRuntime.exec(cmd)
    val timeout = 1.minute
    blocking {
      val exitCode = process.waitFor(timeout.toSeconds, java.util.concurrent.TimeUnit.SECONDS)
      if (exitCode) {
        val output = scala.io.Source.fromInputStream(process.getInputStream).mkString
        val errorOutput = scala.io.Source.fromInputStream(process.getErrorStream).mkString
        if (!output.endsWith("Ok\n")) {
          println(s"output=${output}")
          println(s"error=${errorOutput}")
          throw new RuntimeException(s"Process $cmd failed")
        }
      } else {
        process.destroy()
        throw new RuntimeException(s"Process $cmd timed out")
      }
    }
  }

  @Test
  def testShiftIterableOps(): Unit = {
    val dirname = "testdata/set14runtests/m1"
    val testClassName = "cps.TestBS1ShiftIterableOps"
    compileAndRunTestAfterCommon(dirname, testClassName)
  }


}


object Test14Run {

  var commonCompiled = false

}