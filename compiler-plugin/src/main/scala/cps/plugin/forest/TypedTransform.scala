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
     val nested = RootTransform(typedTerm.expr, owner, nesting+1)
     nested.unpure match
       case Some(value) =>
         if (nested.isOriginEqSync) {
           CpsTree.unchangedPure(typedTerm,owner)
         } else {
           CpsTree.pure(typedTerm, owner, value)
         }
       case None =>
         ???
         /*
          nested.asyncKind match
            case Async(internalKind) =>
              ???
          val changedType = CpsTransformHelper.cpsTransformedType(typedTerm.tpt, summon[CpsTopLevelContext].monadType)
         */
  }

}
