package cps.plugin

import dotty.tools.dotc.core.DenotTransformers.SymTransformer
import dotty.tools.dotc.plugins.PluginPhase
import dotty.tools.dotc.transform.{Erasure,VCElideAllocations}

class PhaseChangeSymbolsAndRemoveScaffolding(val selectedNodes: SelectedNodes,
                                             val shiftedSymbols:ShiftedSymbols
                                            ) extends PluginPhase, SymTransformer,
                                                 CpsChangeSymbols,
                                                 RemoveScaffolding
{

  override def phaseName: String = PhaseChangeSymbolsAndRemoveScaffolding.name

  override val runsAfter = Set(Erasure.name, PhaseCps.name)
  override val runsBefore = Set(VCElideAllocations.name)

  override def changesMembers: Boolean = true
  //override def changesBaseTypes: Boolean = true


}

object PhaseChangeSymbolsAndRemoveScaffolding {
  val name = "rssh.cpsPhaseChangeSymbolsAndRemoveScaffolding"
}