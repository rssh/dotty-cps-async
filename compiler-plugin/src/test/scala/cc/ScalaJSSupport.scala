package cc


import scala.concurrent.Await
import scala.concurrent.duration.Duration
import org.scalajs.linker.{PathIRContainer, PathOutputDirectory, StandardImpl}
import org.scalajs.linker.interface.{Linker, ModuleInitializer, Semantics, StandardConfig}
import org.scalajs.logging.ScalaConsoleLogger
import org.scalajs.jsenv.{Input, RunConfig}
import org.scalajs.jsenv.nodejs.NodeJSEnv

import java.io.{File, InputStream}
import java.nio.file.{Path, Paths}
import scala.util.control.NonFatal


// stolen dotty/sjs-compiler-tests/test/scala/dotty/tools/dotc

object ScalaJSLink {

   def main(args: Array[String]): Unit = {
     val classpath = args(0)
     val mainClass = args(1)
     val outputDir = args(2)
     val jsFile = ScalaJSSupport.link(classpath, mainClass, outputDir)
     println(jsFile)
   }
}

object ScalaJSSupport {

  import scala.concurrent.ExecutionContext.Implicits.global


  def link(classpath: String, mainClass:String, outputDir:String): Path = {
      val outputPath = Paths.get(outputDir)
      val moduleInitializer = ModuleInitializer.mainMethod(mainClass, "main")
      val linkerConfig = StandardConfig().withSemantics(Semantics.Defaults)
      val linker = StandardImpl.linker(linkerConfig)
      val pathes = classpath.split(File.pathSeparator).map(Path.of(_) ).toSeq
      val futureContainers = PathIRContainer.fromClasspath(pathes).map(_._1)
      val irContainers = Await.result(futureContainers, Duration.Inf)
      val irCache = StandardImpl.irFileCache.newCache
      val futureIrFiles = irCache.cached(irContainers)
      val irFiles = Await.result(futureIrFiles, Duration.Inf)
      val linkerOutputDir = PathOutputDirectory(outputPath)
      val logger = new ScalaConsoleLogger(org.scalajs.logging.Level.Debug)
      val reportFuture = linker.link(
        irFiles,
        Seq(moduleInitializer),
        linkerOutputDir,
        logger
      )
      val report = Await.result(reportFuture, Duration.Inf)
      if (report.publicModules.size != 1)
        throw new Exception("Expected exactly one public module")
      val jsFname = report.publicModules.head.jsFileName
      // this pritnln is needed - in forked process we read stdout.
      println(jsFname)
      val retval = outputPath.resolve(jsFname)
      retval
  }

  def forkLink(classpath: String, mainClass:String, outputDir:String): Path = {
    // compiler 3.3.1 have class org.scalajs.ir.Names included, which is incompatible with those we use with linker
    // So, we need to fork a process and exclude compiler from classpath
    //
    // also we need change dotty-cps-async lib from jvm to js
    val jsClasspath = classpath.split(File.pathSeparator)
      .filterNot(_.contains("scala3-compiler_3"))
      .map(replaceDotcCpsAsyncClasses(_))
      .mkString(File.pathSeparator)


    val linkScript = s"java -classpath ${jsClasspath}  cc.ScalaJSLink ${jsClasspath} ${mainClass} ${outputDir}"
    println("ScalaJSLink: linkScript: " + linkScript)
    val timeout = Duration(10, java.util.concurrent.TimeUnit.MINUTES)
    val process = Runtime.getRuntime().exec(linkScript)
    val exit = process.waitFor(timeout.toSeconds, java.util.concurrent.TimeUnit.SECONDS)
    if (exit) {
      val exitCode = process.exitValue()
      val out = process.getInputStream().readAllBytes()
      val err = process.getErrorStream().readAllBytes()
      println("ScalaJSLink: Stdout: " + new String(out))
      println("ScalaJSLink: Stderr: " + new String(err))
      // assume that filename is a last non-empty line of the output
      val stringOutput = new String(out).split('\n').map(_.strip()).filter(_.nonEmpty)
      if (stringOutput.isEmpty)
        throw new RuntimeException("ScalaJSLink: linkScript output is empty")
      val fname = stringOutput.last
      Paths.get(fname)
    } else {
      process.destroy()
      throw new RuntimeException("ScalaJSLink: linkScript timeout")
    }

  }

  def run(classpath: String, mainClass:String, outputDir:String): String = {
    val jsFile = forkLink(classpath, mainClass, outputDir)
    val input = List(Input.Script(jsFile))
    val logger = new ScalaConsoleLogger(org.scalajs.logging.Level.Debug)
    val runner = new NodeJSEnv()
    var stdout: Option[InputStream] = None
    var stderr: Option[InputStream] = None
    val config = RunConfig().withLogger(logger)
      .withInheritOut(false)
      .withInheritErr(false)
      .withOnOutputStream{ (out, err) =>
        stdout = out
        stderr = err
      }

    println(s"ScalaJSRun: Start, input=$input")
    val run = runner.start(input, config)
    var runSuccess = false
    try {
      try {
        Await.result(run.future, Duration.Inf)
        runSuccess = true
      } catch {
        case NonFatal(e) =>
          println("ScalaJSRun: Exception: " + e)
          e.printStackTrace()
          throw e
      }
      val out = stdout.map(input => new String(input.readAllBytes()))
      val err = stderr.map(input => new String(input.readAllBytes()))
      if (!err.isEmpty) {
        println("ScalaJSRun: Stderr: " + err.get)
      }
      out.getOrElse("")
    } finally {
      run.close()
    }


  }

  private def replaceDotcCpsAsyncClasses(str: String): String = {
    // TODO: remove scala version
    str.replace("dotty-cps-async/jvm/target/scala-3.3.1/classes", "dotty-cps-async/js/target/scala-3.3.1/classes")
  }

}


