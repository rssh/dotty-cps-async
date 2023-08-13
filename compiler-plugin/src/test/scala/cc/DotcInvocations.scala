package cc

import scala.concurrent.*
import scala.concurrent.duration.*
import dotty.tools.dotc.*
import core.Contexts.*
import dotty.tools.io.Path
import interfaces.{CompilerCallback, SourceFile}
import reporting.*

import scala.util.matching.Regex


class DotcInvocations(silent: Boolean = false) {

  def compileFiles(files: List[String], outDir: String, extraArgs: List[String]=List.empty, checkAll: Boolean = true): Reporter = {
    val outPath = Path(outDir)
    if (!outPath.exists) outPath.createDirectory()
    val args = List("-d", outDir) ++
             List("-Xplugin:src/main/resources", "-usejavacp") ++
             extraArgs ++
             DotcInvocations.defaultCompileOpts ++
             (if (checkAll) List("-Ycheck:all") else List.empty) ++
             files
    val filledReporter = Main.process(args.toArray, reporter, callback)
    filledReporter
  }

  def compileFilesInDirs(dirs: List[String], outDir: String, extraArgs: List[String] = List.empty, checkAll: Boolean = true): Reporter = {
    val files = dirs.flatMap { dir => scalaFilesIn(Path(dir)) }
    compileFiles(files, outDir, extraArgs, checkAll)
  }

  def compileFilesInDir(dir: String, outDir: String, extraArgs: List[String]=List.empty, checkAll:Boolean = true): Reporter = {
    compileFilesInDirs(List(dir), outDir, extraArgs, checkAll)
  }

  def compileAndRunFilesInDirs(dirs: List[String], outDir: String, mainClass:String = "Main", extraArgs: List[String] = List.empty): (Int,String) = {
    val reporter = compileFilesInDirs(dirs, outDir, extraArgs)
    if (reporter.hasErrors) {
      println(s"Compilation failed in dirs ${dirs}")
      println(reporter.allErrors.mkString("\n"))
      throw new RuntimeException("Compilation failed")
    } else {
      run(outDir, mainClass)
    }
  }

  def compileAndRunFilesInDir(dir: String, outDir: String, mainClass:String = "Main", extraArgs: List[String] = List.empty): (Int,String) = {
    compileAndRunFilesInDirs(List(dir), outDir, mainClass, extraArgs)
  }

  private def run(outDir: String, mainClass: String, timeout: FiniteDuration = 1.minute): (Int, String) = {
    val classpath = s"$outDir:${System.getProperty("java.class.path")}"
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
}


case class TestRun(inputDir: String, mainClass: String, expectedOutput: String = "Ok\n")

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
      "-explain",
      //List("-Yprint-debug") ++
      //List("-Yshow-tree-ids") ++
      //List("-verbose") ++
      //List("-unchecked") ++
       "--color:never",
       "-Vprint:erasure",
       "-Vprint:rssh.cps",
       "-Vprint:inlining"
    //List("-Vprint:constructors") ++
    //List("-Vprint:lambdaLift") ++
    //List("-Xshow-phases") ++

    )
  }


  case class InvocationArgs(
                           extraDotcArgs: List[String] = List.empty,
                           silent: Boolean = false,
                           checkAll: Boolean = true
                           )

  def compileFilesInDir(dir: String, invocationArgs: InvocationArgs = InvocationArgs()): Unit = {
    val dotcInvocations = new DotcInvocations(invocationArgs.silent)
    dotcInvocations.compileFilesInDir(dir,dir,invocationArgs.extraDotcArgs,invocationArgs.checkAll)
    checkReporter(dotcInvocations.reporter)
  }

  def compileAndRunFilesInDirAndCheckResult(
                                  dir: String,
                                  mainClass: String,
                                  expectedOutput: String = "Ok\n",
                                  invocationArgs: InvocationArgs = InvocationArgs()
                                           ): Unit = {
    val dotcInvocations = new DotcInvocations(invocationArgs.silent)

    val (code, output) = dotcInvocations.compileAndRunFilesInDir(dir,dir,mainClass)

    val reporter = dotcInvocations.reporter
    println("summary: " + reporter.summary)
    checkReporter(reporter)

    //println(s"output=${output}")
    assert(output == expectedOutput, s"The output should be '$expectedOutput', we have '$output''")

  }

  def checkRuns(selection: Regex = Regex(".*"), dotcArgs:InvocationArgs = InvocationArgs())(
                 runs: TestRun*
               ): Unit = {
    for(r <- runs) {
       if (selection.matches(r.inputDir)) {
         compileAndRunFilesInDirAndCheckResult(r.inputDir,r.mainClass,r.expectedOutput,dotcArgs)
       }
    }
  }

  private def checkReporter(reporter: Reporter): Unit = {
    if (!reporter.allErrors.isEmpty) {
      for(err <- reporter.allErrors)  {
        println(s"${err.msg} at ${err.pos.source}:${err.pos.line}:${err.pos.column}")
      }
    }
    assert(reporter.allErrors.isEmpty, "There should be no errors")
  }

}