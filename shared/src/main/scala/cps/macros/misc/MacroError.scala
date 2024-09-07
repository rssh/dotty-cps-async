/**
 * part of dotty-cps-async
 * (C) Ruslan Shevchenko, <ruslan@shevchenko.kiev.ua>, 2020-2024
 **/
package cps.macros.misc

import scala.quoted._


case class MacroError(msg: String, posExpr: Expr[?], printed: Boolean = false) extends RuntimeException(msg)


