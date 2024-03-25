package cpstest.stacktraceReporting

import scala.quoted._

case class SourcePos(file: String, line: Int)

object SourcePos {

  inline def current: SourcePos =
    ${ currentImpl }

  def currentImpl(using Quotes): Expr[SourcePos] = {
    import quotes.reflect._
    val pos = Position.ofMacroExpansion
    val file = pos.sourceFile.name
    val line = pos.startLine + 1 // 1-based
    '{ SourcePos(${Expr(file)}, ${Expr(line)}) }
  }

}
