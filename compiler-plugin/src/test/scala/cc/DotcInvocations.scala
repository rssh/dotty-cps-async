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
