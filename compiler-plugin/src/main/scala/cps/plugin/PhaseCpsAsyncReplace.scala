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

// TODO: merge with PhasCps
class PhaseCpsAsyncReplace(selectedNodes: SelectedNodes, shiftedSymbols: ShiftedSymbols)
    extends PluginPhase {

  override val phaseName = PhaseCpsAsyncReplace.name

  // strange -
  override def allowsImplicitSearch = true
  override val runsAfter            = Set(PhaseCpsAsyncShift.name)
  override val runsBefore           = Set(PhaseChangeSymbolsAndRemoveScaffolding.name, Erasure.name)

  /**
   * replaces symbols by transformed values from shiftedSymbols
   * @param tree
   * @param Context
   * @return
   */
  override def transformApply(tree: tpd.Apply)(using Context): Tree =

    //ref(term.symbol) === tree.fun naked from type-aprameters

     //Apply(Plus[String,String],arg)
     //symbol: Plus

    // TODO: look for the shiftedSymbols functions application
    shiftedSymbols.getRecord(tree.fun.symbol) match
      case Some(fun) =>
        val newApply =
          cpy.Apply(tree)(Ident(fun.shiftedMethod.symbol.namedType), tree.args)
        newApply
      case None => super.transformApply(tree)

}

object PhaseCpsAsyncReplace {
  val name: String = "rssh.cpsAsyncReplace"
}
