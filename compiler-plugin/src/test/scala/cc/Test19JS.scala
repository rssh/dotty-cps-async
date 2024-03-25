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

    //val libraryClasspath = currentClasspath.split(":").filter {
    //  x => x.contains("scalajs-library")
    //}.mkString(":")
    val libraryClasspath = currentClasspath

    val inDir = "testdata/set19js/1"
    val outDir = "testdata/set19js/1/out"
    val jsLinkOut = "testdata/set19js/1/linkout"

    val reporter = dotcInvocations.compileFilesInDirsWithFullArgs(List(inDir),  outDir,
      List("-scalajs","-usejavacp","-d", outDir)
    )

    println(s"summary: ${reporter.summary}, errorCount: ${reporter.errorCount}")

    assert(reporter.allErrors.isEmpty, "There should be no errors")
    val mainClass = "jsexample.JSExample1"

    val output = ScalaJSSupport.run(s"${libraryClasspath}:testdata/set19js/1/out", mainClass, jsLinkOut)

    println(s"JSExample1: output=${output}")

   // assert(output.endsWith("OK\n"), s"output=${output}")
  }

}
