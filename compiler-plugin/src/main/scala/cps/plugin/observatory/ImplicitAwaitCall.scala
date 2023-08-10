package cps.plugin.observatory

import dotty.tools.dotc.ast.tpd.*
import dotty.tools.dotc.*
import dotty.tools.dotc.core.*
import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.core.Symbols.*
import dotty.tools.dotc.core.Types.*

import dotty.tools.dotc.core.Decorators.*



object ImplicitAwaitCall {
  def unapply(tree: Tree)(using ctx: Context): Option[(Tree,Tree,Tree,Tree,Tree,Tree)] =
    val retval = tree match
      case Apply(sel: Select, List(arg)) if (sel.name.toSimpleName.toString == "apply") =>
        println("ImplicitAwaitCall: found apply, qualifier=${sel.qualifier.show}")
        sel.qualifier match
          case ImplicitAwaitLambda(tf,ta,tg,gc,conversion) => Some((arg,tf,ta,tg,gc,conversion))
          case _ => None
      case _ => None
    println(s"ImplicitAwaitCall (result=${retval.isDefined}): tree=${tree.show}")
    retval
}

object ImplicitAwaitLambda {

  def unapply(tree: Tree)(using Context): Option[(Tree,Tree,Tree,Tree,Tree)] = {
    val retval = tree match
      case Inlined(call, bindings, ImplicitAwaitLambda(body)) =>
        Some(body)
      case Block(List(), ImplicitAwaitLambda(body)) =>
        Some(body)
      case Block((ddef: DefDef) :: Nil, closure: Closure) if ddef.symbol == closure.meth.symbol =>
        ddef.rhs match
          case Apply(Apply(TypeApply(cnAwait, List(tf,ta,tg)), List(arg)), List(gc, conversion))
            if cnAwait.symbol == Symbols.requiredMethod("cps.await") => Some((tf,ta,tg,gc,conversion))
          case _ => None
      case _ => None
    println(s"check ImplicitAwaitLambda for tree: ${tree.show}, result=${retval.isDefined} ")
    retval
  }

}

