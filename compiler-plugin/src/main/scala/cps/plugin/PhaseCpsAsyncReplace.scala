package cps.plugin

import dotty.tools.dotc.*
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Types.TypeRef
import core.*
import core.Contexts.*
import core.Symbols.*
import core.Types.*
import ast.tpd.*
import transform.{ Erasure, Pickler, PruneErasedDefs }
import plugins.*

class PhaseCpsAsyncReplace(selectedNodes: SelectedNodes, shiftedSymbols: ShiftedSymbols)
    extends PluginPhase {

  override val phaseName = PhaseCpsAsyncReplace.name

  // strange -
  override def allowsImplicitSearch = true
  override val runsAfter            = Set(PhaseCpsAsyncShift.name)
  override val runsBefore           = Set(Erasure.name, PhaseCpsChangeSymbols.name)

  /**
   * replaces symbols by transformed values from shiftedSymbols
   * @param tree
   * @param Context
   * @return
   */
  override def transformApply(tree: Apply)(using Context): Tree =
    shiftedSymbols.getRecord(tree.fun.symbol) match
      case Some(fun) =>
        println(s"asyncReplace::transformApply::selected ${tree}")
        val newApply =
          cpy.Apply(tree)(Ident(fun.shiftedMethod.symbol.namedType), tree.args)
        newApply
      case None => super.transformApply(tree)

}

object PhaseCpsAsyncReplace {
  val name: String = "rssh.cpsAsyncReplace"
}
