package cps.plugin.forest

import dotty.tools.dotc.*
import core.*
import core.Contexts.*
import core.Types.*
import core.Decorators.*
import core.Symbols.*
import ast.tpd.*

import cps.plugin.*

object LiteralTransform {

  def apply(literalTerm: Literal, owner: Symbol, nesting: Int)(using Context, CpsTopLevelContext): CpsTree = {
    CpsTree.unchangedPure(literalTerm, owner)
  }

}
