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
  override def transformTemplate(tree: tpd.Template)(using Context): tpd.Tree =
    // TODO: look for the shiftedSymbols functions
    // TODO: replace shiftedSymbols functions by new values
    tree

}

object PhaseCpsAsyncReplace {
  val name: String = "rssh.cpsAsyncReplace"
}
