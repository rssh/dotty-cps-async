package cps.plugin.forest

import dotty.tools.dotc.*
import core.*
import core.Contexts.*
import core.Types.*
import core.Decorators.*
import core.Symbols.*
import ast.tpd.*

import cps.plugin.*


object SyncTransform {

  def apply(term: Tree, owner: Symbol, nesting: Int)(using Context, CpsTopLevelContext): CpsTree = {
    CpsTree.unchangedPure(term, owner)
  }

}
