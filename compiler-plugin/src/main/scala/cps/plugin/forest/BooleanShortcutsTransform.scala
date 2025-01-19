package cps.plugin.forest

import dotty.tools.dotc.*
import core.*
import core.Contexts.*
import core.Types.*
import core.Decorators.*
import core.Symbols.*
import core.Constants.*
import ast.tpd.*

import cps.plugin.*

object BooleanShortcutsTransform {

  def apply(
      term: Apply,
      owner: Symbol,
      nesting: Int,
      obj: Tree,
      methodSymbol: Symbol
  )(using Context, CpsTopLevelContext): CpsTree = {

    val x = obj
    val y = term.args.head

    val xCps = RootTransform(x, owner, nesting + 1)
    val yCps = RootTransform(y, owner, nesting + 1)

    val retval = (xCps.asyncKind, yCps.asyncKind) match
      case (AsyncKind.Sync, AsyncKind.Sync) =>
        if (xCps.isOriginEqSync && yCps.isOriginEqSync) then CpsTree.unchangedPure(term, owner)
        else
          val nTerm =
            if (xCps.isOriginEqSync) then cpy.Apply(term)(term.fun, List(yCps.unpure.get))
            else if (yCps.isOriginEqSync) then cpy.Apply(term)(Select(obj, methodSymbol.name.toTermName), term.args)
            else cpy.Apply(term)(Select(xCps.unpure.get, methodSymbol.name.toTermName), List(yCps.unpure.get))
          CpsTree.pure(term, owner, nTerm)
      case (AsyncKind.Sync, AsyncKind.Async(internalKind)) =>
        val ifTerm = generateShortcutIf(term, xCps.unpure.get, yCps.transformed, methodSymbol, true)
        CpsTree.impure(term, owner, ifTerm, internalKind)
      case (AsyncKind.Async(internalKind), AsyncKind.Sync) =>
        val sym = newSymbol(owner, "c".toTermName, Flags.EmptyFlags, defn.BooleanType)
        val valDef = ValDef(sym.asTerm, EmptyTree)
        val refSym = ref(sym).withSpan(x.span)
        MapCpsTree(
          term,
          owner,
          xCps,
          MapCpsTreeArgument(
            Some(valDef),
            CpsTree.pure(term, owner, generateShortcutIf(term, refSym, yCps.unpure.get, methodSymbol, false))
          )
        )
      case (AsyncKind.Async(internalKind1), AsyncKind.Async(internalKind2)) =>
        val sym = newSymbol(owner, "c".toTermName, Flags.EmptyFlags, defn.BooleanType)
        val valDef = ValDef(sym.asTerm, EmptyTree)
        val refSym = ref(sym).withSpan(x.span)
        FlatMapCpsTree(
          term,
          owner,
          xCps,
          FlatMapCpsTreeArgument(
            Some(valDef),
            CpsTree.impure(term, owner, generateShortcutIf(term, refSym, yCps.transformed, methodSymbol, true), internalKind2)
          )
        )
      case other =>
        throw CpsTransformException(s"Impossible kind for boolean expression: $other", term)
    retval
  }

  def generateShortcutIf(origin: Tree, x: Tree, y: Tree, methodSymbol: Symbol, asyncConstant: Boolean)(using
      Context,
      CpsTopLevelContext
  ): Tree = {
    if (methodSymbol == defn.Boolean_&&) {
      If(x, y, generateBooleanConstant(origin, false, asyncConstant)).withSpan(origin.span)
    } else {
      If(x, generateBooleanConstant(origin, true, asyncConstant), y).withSpan(origin.span)
    }
  }

  def generateBooleanConstant(origin: Tree, value: Boolean, isAsync: Boolean)(using Context, CpsTopLevelContext): Tree = {
    val sync = Literal(Constant(value)).withSpan(origin.span)
    if (isAsync) then
      Apply(
        TypeApply(
          Select(summon[CpsTopLevelContext].cpsMonadRef, "pure".toTermName),
          List(TypeTree(defn.BooleanType))
        ),
        List(sync)
      ).withSpan(origin.span)
    else sync
  }

}
