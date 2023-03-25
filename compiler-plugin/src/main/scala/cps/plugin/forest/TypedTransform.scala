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
    val nested = RootTransform(typedTerm.expr, owner, nesting + 1)

    val nType = CpsTransformHelper.cpsTransformedType(typedTerm.tpt.tpe, summon[CpsTopLevelContext].monadType)
    nested.typed(typedTerm)
  }

}
