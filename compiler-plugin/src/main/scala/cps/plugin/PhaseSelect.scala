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
  override val runsBefore = Set("rssh.cps")

  override def transformDefDef(tree: tpd.DefDef)(using Context): tpd.Tree = {
      // TODO: skip inline methods
      if (tree.symbol.denot.is(Flags.Inline)) then
        tree
      else
        val topTree = tree
        val optKind = SelectedNodes.detectDefDefSelectKind(tree)
        optKind.foreach{kind =>
          selectedNodes.addDefDef(tree.symbol,kind)
          println(s"phaseSelect, addDefDef for ${tree.symbol}, symbol.id= ${tree.symbol.id}")
        }
        val childTraversor = new TreeTraverser {
          override def traverse(tree: Tree)(using Context): Unit = {
            tree match
              case fun: DefDef if (fun.symbol != topTree.symbol) =>
                selectedNodes.getDefDefRecord(tree.symbol) match
                  case Some(r) =>
                    if (!r.internal) {
                      println(s"selectPhase: set internal for ${fun.symbol.showFullName} (${fun.symbol.id}) at ${tree.srcPos.startPos.show}, r.identity=${System.identityHashCode(r)}")
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

}

object PhaseSelect {

  val phaseName = "rssh.cpsSelect"

}