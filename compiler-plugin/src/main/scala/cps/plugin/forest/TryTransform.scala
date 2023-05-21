package cps.plugin.forest

import dotty.tools.dotc.*
import core.*
import core.Contexts.*
import core.Types.*
import core.Decorators.*
import core.Symbols.*
import ast.tpd.*
import cps.plugin.*
import cps.plugin.forest.cases.*


object TryTransform {

  def apply(tryTerm:Try, owner: Symbol, nesting:Int)(using Context, CpsTopLevelContext): CpsTree = {
      val cpsExpr = RootTransform(tryTerm.expr, owner, nesting+1)
      val cpsCases = CpsCases.create(tryTerm.cases, owner, nesting+1)
      val casesAsyncKind = cpsCases.collectAsyncKind
      val cpsFinalies: Option[CpsTree] =
        if ! tryTerm.finalizer.isEmpty then
          Some(RootTransform(tryTerm.finalizer, owner, nesting+1))
        else
          None
      ???
  }



}
