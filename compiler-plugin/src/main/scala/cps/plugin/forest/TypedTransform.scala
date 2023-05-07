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

  def apply(typedTerm: Typed, owner: Symbol, nesting: Int)(using Context, CpsTopLevelContext): CpsTree = {
    Log.trace(s"TypedTransform typedTerm=${typedTerm.show}",nesting)
    val nested = RootTransform(typedTerm.expr, owner, nesting + 1)
    val retval = nested.asyncKind match {
      case AsyncKind.Sync =>
        if (nested.isOriginEqSync)
          CpsTree.unchangedPure(typedTerm,owner)
        else {
          val nestedTree = nested.unpure.get
          val newTypedTerm = Typed(nestedTree, typedTerm.tpt).withSpan(typedTerm.span)
          CpsTree.pure(typedTerm, owner, newTypedTerm)
        }
      case _ =>
        nested.typed(typedTerm)
    }
    Log.trace(s"TypedTransform retval=${retval.show}, asyncKind=${retval.asyncKind}",nesting)
    retval
  }

}
