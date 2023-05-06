package cps.plugin.forest

import dotty.tools.dotc.*
import core.*
import core.Contexts.*
import core.Types.*
import core.Symbols.*
import core.Decorators.*
import core.Definitions.*
import core.StdNames
import ast.tpd.*

import cps.plugin.*


object TypedTransform {

  def apply(typedTerm: Typed, oldOwner: Symbol, newOwner: Symbol, nesting: Int)(using Context, CpsTopLevelContext): CpsTree = {
    val nested = RootTransform(typedTerm.expr, oldOwner, newOwner, nesting + 1)
    nested.asyncKind match {
      case AsyncKind.Sync =>
        if (nested.isOriginEqSync)
          CpsTree.unchangedPure(typedTerm.changeOwner(oldOwner,newOwner),newOwner)
        else {
          val nestedTree = nested.unpure.get
          val newTypedTerm = Typed(nestedTree, typedTerm.tpt).withSpan(typedTerm.span)
          CpsTree.pure(typedTerm, newOwner, newTypedTerm)
        }
      case _ =>
        nested.typed(typedTerm.changeOwner(oldOwner,newOwner))
    }
  }

}
