package cps.plugin

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.*
import ast.tpd.*
import core.*
import core.Symbols.*
import core.Types.*
import plugins.*
import cps.plugin.DefDefSelectKind.USING_CONTEXT_PARAM
import dotty.tools.dotc.transform.{Pickler, SetRootTree}


class PhaseSelect(selectedNodes: SelectedNodes) extends PluginPhase {

  val phaseName = PhaseSelect.phaseName

  override val runsAfter = Set(SetRootTree.name, Pickler.name)
  override val runsBefore = Set(PhaseCps.name)

  override def transformDefDef(tree: tpd.DefDef)(using Context): tpd.Tree = {
      if (tree.symbol.denot.is(Flags.Inline)) then
        tree
      else
        val topTree = tree
        val optKind = SelectedNodes.detectDefDefSelectKind(tree)
        optKind.foreach{kind =>
          selectedNodes.addDefDef(tree.symbol,kind)
        }
        // TODO:  try run this onlu on selected nodes
        val childTraversor = new TreeTraverser {
          override def traverse(tree: Tree)(using Context): Unit = {
            tree match
              case fun: DefDef if (fun.symbol != topTree.symbol) =>
                selectedNodes.getDefDefRecord(tree.symbol) match
                  case Some(r) =>
                    if (!r.internal) {
                      selectedNodes.markAsInternal(tree.symbol)
                      traverseChildren(tree)
                    }
                  case None =>
                    traverseChildren(tree)
              case Block(List(ddef:DefDef), closure:Closure) if ddef.symbol == closure.meth.symbol =>
                traverseChildren(tree)
              case Block(List(ddef:DefDef), Typed(closure:Closure, tp)) if ddef.symbol == closure.meth.symbol =>
                traverseChildren(tree)
              case Block(stats, expr) =>
                // don't mark local function definitions and templates as internal
                for(s <- stats) {
                  s match
                    case defDef: DefDef  =>
                        //traverse(defDef)
                    case tdef: TypeDef =>
                        // do nothing
                    case other =>
                        traverse(other)
                }
                traverse(expr)
              case _ =>
                traverseChildren(tree)
          }
        }
        childTraversor.traverse(tree)
        tree
  }

  override def transformValDef(tree: tpd.ValDef)(using Context): tpd.Tree = {
    tree.rhs match
      case EmptyTree =>
        tree
      case other =>
        if (!tree.symbol.flags.isOneOf(Flags.InlineOrProxy|Flags.Synthetic) &&
            CpsTransformHelper.isCpsDirectType(tree.rhs.tpe)) then
              report.error("CpsDirect can't be a value", tree.srcPos)
              tree
        else
          super.transformValDef(tree)
  }

  override def transformAssign(tree: tpd.Assign)(using Context): tpd.Tree = {
    if (CpsTransformHelper.isCpsDirectType(tree.tpe)) then
      report.error("CpsDirect can't be a assigned", tree.srcPos)
      tree
    else
      super.transformAssign(tree)
  }


}

object PhaseSelect {

  val phaseName = "rssh.cpsSelect"

}