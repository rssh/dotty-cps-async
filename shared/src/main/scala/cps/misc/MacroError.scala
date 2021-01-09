package cps.misc

import scala.quoted._

case class MacroError(msg: String, posExpr: Expr[_], printed: Boolean = false) extends RuntimeException(msg)


