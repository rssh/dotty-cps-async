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
      println("ScalaJSLink: JsFile: " + jsFname)
      val retval = outputPath.resolve(jsFname)
      retval
  }


  def run(classpath: String, mainClass:String, outputDir:String): String = {
    val jsFile = link(classpath, mainClass, outputDir)
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

    val run = new NodeJSEnv().start(input, config)
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

}


