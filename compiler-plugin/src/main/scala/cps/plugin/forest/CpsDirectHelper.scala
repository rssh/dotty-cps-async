package cps.plugin.forest

import dotty.tools.dotc.*
import ast.tpd.*
import core.{Symbols, *}
import core.Contexts.*
import core.Decorators.*
import core.Names.*
import core.StdNames.*
import core.Symbols.*
import core.SymDenotations.*
import util.Spans.Span
import core.Types.*
import core.Phases.*
import cps.plugin.*
import cps.{CpsMonadContext, CpsMonadConversion}
import inlines.Inlines
import transform.Inlining


object CpsDirectHelper {

  /**
   * generate new CpsDirect[tf](fctx)  (positioned at posTerm)
   * @param tf - type of wrapper monad 
   * @param fctx - current monnad context
   * @param posTerm - position of new term
   * @return new CpsDirect[tf](fctx)
   */
  def genCpsDirectDefaultConstructor(tf: TypeTree, fctx: Tree, posTerm: Tree)(using Context): Tree =
    val cpsDirectType = Symbols.requiredClassRef("cps.CpsDirect").appliedTo(tf.tpe)
    val cpsDirectConstructor = Select(New(TypeTree(cpsDirectType)), "<init>".toTermName)
    Apply(
      TypeApply(cpsDirectConstructor, List(tf)),
      List(fctx)
    ).withSpan(posTerm.span)


  def substituteCpsDirectArgInApply(applyTerm: Apply, originCPsDirectArg: Tree, nCpsDirectArg: Tree)(using Context): Apply = {
    Apply(
      applyTerm.fun,
      applyTerm.args.map(
        arg => if (arg eq originCPsDirectArg) then nCpsDirectArg.withSpan(arg.span) else arg
      )
    ).withSpan(applyTerm.span)
  }

}
