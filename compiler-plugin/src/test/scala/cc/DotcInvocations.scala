package cc

import scala.concurrent.*
import scala.concurrent.duration.*
import dotty.tools.dotc.*
import core.Contexts.*
import dotty.tools.io.Path
import interfaces.{CompilerCallback, SourceFile}
import reporting.*

import scala.util.matching.Regex


class DotcInvocations(silent: Boolean = true, scalaJs: Boolean = false) {

  def compileFiles(files: List[String], outDir: String, extraArgs: List[String]=List.empty, checkAll: Boolean = true, usePlugin: Boolean=true): Reporter = {
    val args = List("-d", outDir) ++
             (if (usePlugin) then List("-Xplugin:src/main/resources") else List.empty) ++
             compilerClasspathOption ++
             extraArgs ++
             DotcInvocations.defaultCompileOpts ++
             (if (checkAll) List("-Ycheck:all") else List.empty)
    println(s"compile args: ${args}, usePlugin=${usePlugin}")
    compileFilesWithFullArgs(files, outDir, args)
  }

  def compileFilesWithFullArgs(files: List[String], outDir: String, args: List[String]): Reporter = {
    val outPath = Path(outDir)
    if (!outPath.exists) outPath.createDirectory()
    val argsWithFiles = args ++ files
    val filledReporter = Main.process(argsWithFiles.toArray, reporter, callback)
    filledReporter
  }

  def compileFilesInDirsWithFullArgs(dirs: List[String], outDir: String, args: List[String]): Reporter = {
    println(s"dirs: ${dirs}")
    val files = dirs.flatMap { dir => scalaFilesIn(Path(dir)) }
    println("files: " + files.mkString(" "))
    compileFilesWithFullArgs(files, outDir, args)
  }


  def compileFilesInDirs(dirs: List[String], outDir: String, extraArgs: List[String] = List.empty, checkAll: Boolean = true, usePlugin: Boolean=true): Reporter = {
    val files = dirs.flatMap { dir => scalaFilesIn(Path(dir)) }
    compileFiles(files, outDir, extraArgs, checkAll, usePlugin)
  }

  def compileFilesInDir(dir: String, outDir: String, extraArgs: List[String]=List.empty, checkAll:Boolean = true, usePlugin: Boolean = true): Reporter = {
    compileFilesInDirs(List(dir), outDir, extraArgs, checkAll, usePlugin)
  }



  def compileAndRunFilesInDirsJVM(dirs: List[String], outDir: String, mainClass:String = "Main", extraArgs: List[String] = List.empty, checkAll: Boolean=true, usePlugin: Boolean=true): (Int,String) = {
    val reporter = compileFilesInDirs(dirs, outDir, extraArgs, checkAll, usePlugin)
    if (reporter.hasErrors) {
      println(s"Compilation failed in dirs ${dirs}")
      DotcInvocations.reportErrors(reporter)
      (reporter.errorCount, reporter.allErrors.map(_.msg).mkString("\n"))
    } else {
      runJVM(outDir, mainClass)
    }
  }


  def compileAndRunFilesInDirJVM(dir: String, outDir: String, mainClass:String = "Main", extraArgs: List[String] = List.empty, checkAll: Boolean=true, usePlugin: Boolean=true): (Int,String) = {
    compileAndRunFilesInDirsJVM(List(dir), outDir, mainClass, extraArgs, checkAll, usePlugin)
  }

  private def runJVM(outDir: String, mainClass: String, timeout: FiniteDuration = 1.minute): (Int, String) = {
    val classpath = s"$outDir:${System.getProperty("java.class.path")}"
    DotcInvocations.runJVMInClasspath(mainClass, classpath, timeout)
  }


  /**
   * Recursively list all scala files in a given path
   * @param path
   * @return list of scala files
   */
  def scalaFilesIn(path: Path): List[String] = {
    if (path.isDirectory) {
      val dir = path.toDirectory
      dir.list.toList.flatMap(scalaFilesIn)
    } else if (path.isFile && path.hasExtension("scala")) {
      List(path.toString)
    } else {
      Nil
    }
  }

  val reporter = new Reporter {
    override def doReport(d: Diagnostic)(implicit ctx: Context): Unit = {
      if (!silent) {
         println(d)
      }
    }
  }

  val callback = new CompilerCallback {
    override def onSourceCompiled(source: SourceFile): Unit = {
      if (!silent) {
        println(s"Compiled ${source.jfile}")
      }
    }

  }

  private def compilerClasspathOption: List[String] = {
    if (scalaJs) {
      List("-classpath", DotcInvocations.currentJsClasspath)
    } else {
      List("-usejavacp")
    }
  }

}


case class TestRun(inputDir: String, mainClass: String, expectedOutput: String = "Ok\n", extraDotcArgs:List[String] = List.empty)




case class DotcInvocationArgs(
                               extraDotcArgs: List[String] = List.empty,
                               silent: Boolean = false,
                               checkAll: Boolean = true,
                               usePlugin: Boolean = true,
                               useScalaJsLib: Boolean = false,
                               outDir: Option[String] = None,
                             )





object DotcInvocations {

  import org.junit.Assert.*

  val defaultCompileOpts: List[String] = {
    // note, that -Ycheck:all is not included here, because it is added conditionally
    List(
      //"-Ydebug-error",
      //"--unique-id",
      //"-Xcheck-macros",
      "-Ydebug",
      //"-Yprint-syms",
      //"-explain",
      //List("-Yprint-debug") ++
      //List("-Yshow-tree-ids") ++
      //List("-verbose") ++
      //List("-unchecked") ++
       "--color:never",
      // "-Vprint:erasure",
      // "-Vprint:rssh.cps",
      //"-Vprint:inlining"
      //List("-Vprint:constructors") ++
      //List("-Vprint:lambdaLift") ++
      //List("-Xshow-phases") ++
    )
  }





  def compileFilesInDir(dir: String, invocationArgs: DotcInvocationArgs = DotcInvocationArgs()): Reporter = {
    val dotcInvocations = new DotcInvocations(invocationArgs.silent)
    dotcInvocations.compileFilesInDir(dir, invocationArgs.outDir.getOrElse(dir),
      invocationArgs.extraDotcArgs, invocationArgs.checkAll, invocationArgs.usePlugin)
    dotcInvocations.reporter
  }


  def succesfullyCompileFilesInDir(dir: String, invocationArgs: DotcInvocationArgs = DotcInvocationArgs()): Unit = {
    val reporter = compileFilesInDir(dir, invocationArgs)
    checkReporter(reporter)
  }

  def compileAndRunFilesInDirAndCheckResult(
                                  dir: String,
                                  mainClass: String,
                                  expectedOutput: String = "Ok\n",
                                  invocationArgs: DotcInvocationArgs = DotcInvocationArgs()
                                           ): Unit = {
    val dotcInvocations = new DotcInvocations(invocationArgs.silent)

    val (code, output) = dotcInvocations.compileAndRunFilesInDirJVM(dir,invocationArgs.outDir.getOrElse(dir),
      mainClass,invocationArgs.extraDotcArgs,invocationArgs.checkAll,invocationArgs.usePlugin)

    val reporter = dotcInvocations.reporter
    println("summary: " + reporter.summary)
    checkReporter(reporter)

    //println(s"output=${output}")
    assert(output.endsWith(expectedOutput), s"The output should ends with '$expectedOutput', we have '$output''")

  }



  def checkRuns(selection: Regex = Regex(".*"), dotcArgs: DotcInvocationArgs = DotcInvocationArgs())(
                 runs: TestRun*
               ): Unit = {
    for(r <- runs) {
       if (selection.matches(r.inputDir)) {
         compileAndRunFilesInDirAndCheckResult(r.inputDir,r.mainClass,r.expectedOutput,dotcArgs.copy(extraDotcArgs = dotcArgs.extraDotcArgs ++ r.extraDotcArgs))
       }
    }
  }

  trait IsAlreadyCompiledFlag {
    var isAlreadyCompiled: Boolean
  }


  case class Dependency(
                         sourceDir: String,
                         compiledFlag: IsAlreadyCompiledFlag
                       ) {
    def outDir = sourceDir
  }


  def compileAndRunJunitTestAfterDependency(dirname: String,
                                            testClassName: String,
                                            invocationArgs: DotcInvocationArgs = DotcInvocationArgs(),
                                            dependency: Dependency
                                  ): Unit = {
    if (!dependency.compiledFlag.isAlreadyCompiled) {
      DotcInvocations.succesfullyCompileFilesInDir(dependency.sourceDir, invocationArgs)
      dependency.compiledFlag.isAlreadyCompiled = true
      println("-----finish common compilation-----")
    }
    val baseClassPath = if (invocationArgs.useScalaJsLib) currentJsClasspath else System.getProperty("java.class.path")
    val classpath1 = s"${dependency.outDir}:${baseClassPath}"
    val secondInvokationArgs = invocationArgs.copy(extraDotcArgs = List("-classpath", classpath1) ++ invocationArgs.extraDotcArgs)
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

  def runJVMInClasspath(mainClass: String, classpath: String, timeout: FiniteDuration = 1.minute): (Int, String) = {
    val cmd = s"java -cp $classpath $mainClass"
    println(s"Running $cmd")
    val process = Runtime.getRuntime.exec(cmd)
    blocking {
      val exitCode = process.waitFor(timeout.toSeconds, java.util.concurrent.TimeUnit.SECONDS)
      if (exitCode) {
        val output = scala.io.Source.fromInputStream(process.getInputStream).mkString
        val errorOutput = scala.io.Source.fromInputStream(process.getErrorStream).mkString
        println(s"output=${output}")
        println(s"error=${errorOutput}")
        (process.exitValue(), output)
      } else {
        process.destroy()
        throw new RuntimeException(s"Process $cmd timed out")
      }
    }
  }


  def reportErrors(reporter: Reporter): Unit = {
    if (!reporter.allErrors.isEmpty) {
      for (err <- reporter.allErrors) {
        println(s"${err.msg} at ${err.pos.source}:${err.pos.line}:${err.pos.column}")
      }
    }
  }

  private def checkReporter(reporter: Reporter): Unit = {
    reportErrors(reporter)
    assert(reporter.allErrors.isEmpty, "There should be no errors")
  }

  private def currentJsClasspath: String = {
    // substitue the jvm cps classes to js cps classes
    val classpath = System.getProperty("java.class.path")
    val jsClasspath = classpath.replaceAll("dotty-cps-async/jvm/target/scala-3.3.3/classes", "dotty-cps-async/js/target/scala-3.3.3/classes")
    jsClasspath
  }

}