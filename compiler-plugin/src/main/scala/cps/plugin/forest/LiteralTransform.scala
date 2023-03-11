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


  def apply(literalTerm: Literal, owner: Symbol, tctx: TransformationContext, nesting:Int)(using Context): CpsTree = {
    CpsTree.unchangedPure(tctx,literalTerm,owner)
  }


}