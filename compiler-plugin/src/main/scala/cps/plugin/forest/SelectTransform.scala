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


  def apply(selectTerm: Select, owner: Symbol, ctx: TransformationContext, nesting:Int)(using Context): CpsTree = {
    val cpsQualifier = RootTransform(selectTerm.qualifier, owner, ctx,nesting+1)
    cpsQualifier.select(selectTerm.name,selectTerm,selectTerm.tpe)
  }


}