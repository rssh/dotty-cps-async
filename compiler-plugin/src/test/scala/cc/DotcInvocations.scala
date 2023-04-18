package cc

import dotty.tools.dotc.*
import core.Contexts.*
import dotty.tools.io.Path
import interfaces.{CompilerCallback, SourceFile}
import reporting.*

class DotcInvocations {

  def compileFiles(files: List[String], outDir: String, extraArgs: List[String]=List.empty): Reporter = {
    val outPath = Path(outDir)
    if (!outPath.exists) outPath.createDirectory()
    val args = List("-d", outDir) ++
             extraArgs ++
             List("-Xplugin:src/main/resources", "-usejavacp") ++
             List("-Ycheck:rssh.cps") ++
             //List("-Yprint-syms") ++
             //List("-Yprint-debug") ++
             //List("-Yshow-tree-ids") ++
             //List("-verbose") ++
             //List("-unchecked") ++
             List("--color:never") ++
             List("-Vprint:rssh.cps") ++
             //List("-Vprint:inlining") ++
             //List("-Vprint:constructors") ++
             //List("-Vprint:lambdaLift") ++
             files
    val filledReporter = Main.process(args.toArray, reporter, callback)
    filledReporter
  }

  def compileFilesInDirs(dirs: List[String], outDir: String, extraArgs: List[String] = List.empty): Reporter = {
    val files = dirs.flatMap { dir => scalaFilesIn(Path(dir)) }
    compileFiles(files, outDir, extraArgs)
  }

  def compileFilesInDir(dir: String, outDir: String, extraArgs: List[String]=List.empty): Reporter = {
    compileFilesInDirs(List(dir), outDir, extraArgs)
  }

  def compileAndRunFilesInDirs(dirs: List[String], outDir: String, mainClass:String = "Main", extraArgs: List[String] = List.empty): String = {
    val reporter = compileFilesInDirs(dirs, outDir, extraArgs)
    if (reporter.hasErrors) {
      println("Compilation failed")
      println(reporter.allErrors.mkString("\n"))
      throw new RuntimeException("Compilation failed")
    } else {
      run(outDir, mainClass)
    }
  }

  private def run(outDir: String, mainClass: String): String = {
    val classpath = s"$outDir:${System.getProperty("java.class.path")}"
    val cmd = s"java -cp $classpath $mainClass"
    println(s"Running $cmd")
    val process = Runtime.getRuntime.exec(cmd)
    val output = scala.io.Source.fromInputStream(process.getInputStream).mkString
    println(output)
    output
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
      println(d)
    }
  }
  val callback = new CompilerCallback {

    override def onSourceCompiled(source: SourceFile): Unit = {
      println(s"Compiled ${source.jfile}")
    }

  }
}
