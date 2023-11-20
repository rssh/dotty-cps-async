package cc


import dotty.tools.dotc.reporting.Reporter

import scala.concurrent.*
import scala.concurrent.duration.*

import org.junit.Test

class Test19JS {

  @Test
  def testCompileAndRunExample2(): Unit = {
    val dotcInvocations = new DotcInvocations(false)

    val currentClasspath = System.getProperty("java.class.path")
    println(s"currentClasspath=${currentClasspath.split(":").mkString("\n")}")
    val filteredClasspath = currentClasspath.split(":").filterNot{
      x => x.contains("scalajs") && !(x.contains("scalajs-library"))
    }.mkString(":")

    val libraryClasspath = currentClasspath.split(":").filter {
      x => x.contains("scalajs-library")
    }.mkString(":")

    val cmdScript =
      s"java -classpath ${filteredClasspath} dotty.tools.dotc.Main -scalajs -classpath ${libraryClasspath}  -d testdata/set19js/1/out testdata/set19js/1/JSExample1.scala"
     println(s"cmdScript=${cmdScript}")

    val reporter = dotcInvocations.compileFilesInDirsWithFullArgs(List("testdata/set19js/1"),  "testdata/set19js/1/out",
      List("-scalajs","-usejavacp","-d", "testdata/set19js/1/out")
    )


    println("summary: " + reporter.summary)

    assert(reporter.allErrors.isEmpty, "There should be no errors")
    val mainClass = "jsexample.JSExample1"

   // val output = ScalaJSSupport.run("testdata/set19js/1/out", mainClass, "testdata/set19js/1/out" )

   // println(s"JSExample1: output=${output}")

   // assert(output.endsWith("OK\n"), s"output=${output}")
  }

}
