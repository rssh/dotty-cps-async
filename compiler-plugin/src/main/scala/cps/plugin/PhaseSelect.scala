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


class PhaseSelect(selectedNodes: SelectedNodes) extends PluginPhase {

  val phaseName = PhaseSelect.phaseName

  override val runsAfter = Set("cc")
  override val runsBefore = Set("rssh.cps")

  override def transformDefDef(tree: tpd.DefDef)(using Context): tpd.Tree = {
      val topTree = tree
      val optKind = SelectedNodes.checkAndProcessDefDef(tree){
        (tree, monadContext) => Some(USING_CONTEXT_PARAM(monadContext))
      } {
        (tree, kind) => Some(DefDefSelectKind.RETURN_CONTEXT_FUN(kind))
      }
      optKind.foreach(kind => selectedNodes.addDefDef(tree.symbol,kind))
      val childTraversor = new TreeTraverser {
        override def traverse(tree: Tree)(using Context): Unit = {
            tree match
              case fun: DefDef if (fun.symbol != topTree.symbol) =>
                selectedNodes.getDefDefRecord(tree.symbol) match
                  case Some(r) =>
                    println(s"selectPhase: set internal for ${fun.symbol.showFullName}")
                    r.internal = true
                  case None => traverseChildren(tree)
              case _ =>
                traverseChildren(tree)
        }
      }
      childTraversor.traverse(tree)
      tree
  }

}

object PhaseSelect {

  val phaseName = "rssh.cpsSelect"




}