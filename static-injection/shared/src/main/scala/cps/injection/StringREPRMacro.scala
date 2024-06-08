package cps.injection

import scala.quoted.*


object StringREPRMacro {

  def applyImpl[T: Type](using qctx: Quotes) = {
    import qctx.reflect.*
    val str = Type.show[T]
    Expr(str)
  }

  

}
