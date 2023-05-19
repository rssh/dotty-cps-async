package cps.plugin.forest

import cps.plugin.*
import dotty.tools.dotc.*
import dotty.tools.dotc.ast.tpd.*
import dotty.tools.dotc.core.*
import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.core.Decorators.*
import dotty.tools.dotc.core.Symbols.*

object AssignTransform {

  def apply(term: Assign, owner: Symbol, nesting: Int)(using Context, CpsTopLevelContext): CpsTree = {
    term.lhs match
      case sel: Select =>
        val lhsQual = RootTransform(sel.qualifier, owner, nesting + 1)
        lhsQual.asyncKind match
          case AsyncKind.Sync =>
            lhsSyncAssign(term, owner, nesting)
          case AsyncKind.Async(internalKind) =>
            lhsSelectAsyncAssign(term, sel, lhsQual, owner, nesting)
          case AsyncKind.AsyncLambda(body) =>
            throw CpsTransformException(s"Can't assign sync value to async lambda", term.srcPos)
      case _ =>
        val cpsLhs = RootTransform(term.lhs, owner, nesting)
        cpsLhs.asyncKind match
          case AsyncKind.Sync =>
            lhsSyncAssign(term, owner, nesting)
          case AsyncKind.Async(internalKind) =>
            throw CpsTransformException(s"Impossible: async assignmebt which is not select, $cpsLhs", term.srcPos)
          case AsyncKind.AsyncLambda(body) =>
            throw CpsTransformException(s"Can't assign value to async lambda", term.srcPos)
  }

  def lhsSyncAssign(term: Assign, owner: Symbol, nesting: Int)(using Context, CpsTopLevelContext): CpsTree = {
    val cpsRhs = RootTransform(term.rhs, owner, nesting)
    cpsRhs.asyncKind match
      case AsyncKind.Sync =>
        if (cpsRhs.isOriginEqSync) {
          CpsTree.unchangedPure(term, owner)
        } else {
          CpsTree.pure(term, owner, Assign(term.lhs, cpsRhs.unpure.get).withSpan(term.span))
        }
      case AsyncKind.Async(internalKind) =>
        val sym = newSymbol(owner, "xAssign".toTermName, Flags.EmptyFlags, term.rhs.tpe.widen, Symbols.NoSymbol)
        val valDef = ValDef(sym.asTerm, EmptyTree)
        val nAssign = Assign(term.lhs, ref(sym)).withSpan(term.span)
        MapCpsTree(term, owner, cpsRhs,
          MapCpsTreeArgument(Some(valDef), CpsTree.unchangedPure(nAssign, owner)))
      case AsyncKind.AsyncLambda(body) =>
        throw CpsTransformException(s"Can't assign async lambda", term.srcPos)
  }

  def lhsSelectAsyncAssign(term: Assign, lhsSelect: Select, lhsQual: CpsTree, owner: Symbol, nesting: Int)(using Context, CpsTopLevelContext): CpsTree = {
    val qualSym = newSymbol(owner, "xQualAssign".toTermName, Flags.EmptyFlags, lhsQual.originType, NoSymbol)
    val qualValDef = ValDef(qualSym.asTerm, EmptyTree)
    val cpsRhs = RootTransform(term.rhs, owner, nesting)
    cpsRhs.asyncKind match
      case AsyncKind.Sync =>
        val nAssign = Assign(
                         ref(qualSym).select(lhsSelect.symbol).withSpan(term.lhs.span),
                         cpsRhs.unpure.get
                      ).withSpan(term.span)
        MapCpsTree(term, owner, lhsQual,
          MapCpsTreeArgument(Some(qualValDef), CpsTree.pure(term, owner, nAssign)))
      case AsyncKind.Async(internalKind) =>
        val rhsSym = newSymbol(owner, "xAssign".toTermName, Flags.EmptyFlags, cpsRhs.originType.widen, NoSymbol)
        val rhsValDef = ValDef(rhsSym, EmptyTree)
        val nAssign = Assign(ref(qualSym).select(lhsSelect.symbol).withSpan(term.lhs.span),
                             ref(rhsSym)
                      ).withSpan(term.span)
        FlatMapCpsTree(term, owner, lhsQual,
          FlatMapCpsTreeArgument(Some(qualValDef),
             MapCpsTree(term, owner, cpsRhs,
               MapCpsTreeArgument(Some(rhsValDef), CpsTree.unchangedPure(nAssign, owner))
             )
          )
        )
      case AsyncKind.AsyncLambda(bodyKind) =>
        throw CpsTransformException(s"Can't assign async lambda", term.srcPos)
  }

}
