package cps.plugin.forest

import dotty.tools.dotc.*
import core.*
import core.Contexts.*
import core.Types.*
import core.Decorators.*
import core.Symbols.*
import ast.tpd.*

import cps.plugin.*

object SelectTransform {

  def apply(selectTerm: Select, owner: Symbol, nesting: Int)(using Context, CpsTopLevelContext): CpsTree = {
    val cpsQualifier = RootTransform(selectTerm.qualifier, owner, nesting + 1)
    val retval = cpsQualifier.select(selectTerm)
    retval
  }

}
