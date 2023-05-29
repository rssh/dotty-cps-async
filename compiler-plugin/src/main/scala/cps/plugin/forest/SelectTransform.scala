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


  def apply(selectTerm: Select, owner: Symbol, nesting:Int)(using Context, CpsTopLevelContext): CpsTree = {
      Log.trace(s"SelectTransform, origin=${selectTerm.show}", nesting)
      Log.trace(s"SelectTransform, origin.type=${selectTerm.tpe.widen.show}, qualifier.type=${selectTerm.qualifier.tpe.widen}", nesting)
      val cpsQualifier = RootTransform(selectTerm.qualifier, owner, nesting+1)
      Log.trace(s"SelectTramsform: qualifier=${cpsQualifier.show}",nesting)
      Log.trace(s"SelectTramsform: qualifier.transformed=${cpsQualifier.transformed.show}", nesting)
      Log.trace(s"SelectTramsform: qualifier.transformed.type=${cpsQualifier.transformed.tpe.widen.show}", nesting)
      val retval = cpsQualifier.select(selectTerm)
      Log.trace(s"SelectTransform,  result=${retval.show}", nesting)
      Log.trace(s"SelectTransform,  result.transformed=${retval.transformed.show}", nesting)
      retval
  }


}