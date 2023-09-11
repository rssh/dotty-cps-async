package cc

import org.junit.Test
import org.junit.Assert._
import scala.concurrent.*
import scala.concurrent.duration.*

class Test15PE {

  def compileCommon(): Unit = {
    if (!Test15PE.commonCompiled) {
      DotcInvocations.succesfullyCompileFilesInDir("testdata/set15pe/common")
      Test15PE.commonCompiled = true
      println("-----finish common compilation-----")
    }
  }

  def compileAndRunTestAfterCommon(dirname: String, testClassName: String, invocationArgs: DotcInvocations.InvocationArgs = DotcInvocations.InvocationArgs()): Unit = {
    compileCommon()
    val classpath1 = s"testdata/set15pe/common:${System.getProperty("java.class.path")}"
    val secondInvokationArgs = invocationArgs.copy(extraDotcArgs = List("-classpath", classpath1)++invocationArgs.extraDotcArgs)
    DotcInvocations.succesfullyCompileFilesInDir(dirname, secondInvokationArgs)
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
        val output = scala.io.Source.fromInputStream(process.getInputStream).mkString
        val errorOutput = scala.io.Source.fromInputStream(process.getErrorStream).mkString
        println(s"output=${output}")
        println(s"error=${errorOutput}")
        process.destroy()
        throw new RuntimeException(s"Process $cmd timed out")
      }
    }
  }



  @Test
  def testFizzBuzz() = {
    compileAndRunTestAfterCommon("testdata/set15pe/m1", "cps.pe.TestFizzBuzz",
      DotcInvocations.InvocationArgs(checkAll = true,
        extraDotcArgs = List(
          "-Vprint:rssh.cps"
        )
      )
    )
  }





  @Test
  def testFizzBuzzLocalDirectRef() = {
    compileAndRunTestAfterCommon("testdata/set15pe/m2", "cps.pe.TestFizzBuzz",
      DotcInvocations.InvocationArgs(checkAll = true,
        extraDotcArgs = List(
          "-Vprint:rssh.cps", "-Vprint:erasure"
        )
      )
    )
  }


  @Test
  def testUseResourceM3() = {
    compileAndRunTestAfterCommon("testdata/set15pe/m3", "cps.pe.TestUseResource",
      DotcInvocations.InvocationArgs(checkAll = true,
        extraDotcArgs = List(
         // "-Vprint:rssh.cps", "-Vprint:erasure"
        )
      )
    )
  }


  @Test
  def testUseResourceM4() = {
    compileAndRunTestAfterCommon("testdata/set15pe/m4", "cps.pe.TestUseResourceFromPlugin",
      DotcInvocations.InvocationArgs(checkAll = true,
        extraDotcArgs = List(
         //  "-Vprint:rssh.cps", "-Vprint:erasure"
        )
      )
    )
  }

   

  @Test
  def testFizzBuzzWithInlineDirectRefM5() = {
    compileAndRunTestAfterCommon("testdata/set15pe/m5", "cps.pe.TestFizzBuzzM5",
      DotcInvocations.InvocationArgs(checkAll = false,
        extraDotcArgs = List(
          "-Vprint:rssh.cps", "-Vprint:erasure"
        )
      )
    )
  }


}

object Test15PE {

  var commonCompiled = false

}
