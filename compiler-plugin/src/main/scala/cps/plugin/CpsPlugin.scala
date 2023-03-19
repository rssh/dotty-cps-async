/**
 * Cps compiler plugin.
 **/
package cps.plugin

import dotty.tools.dotc.*
import dotty.tools.dotc.plugins.*

class CpsPlugin extends StandardPlugin {

  override val name        = "rssh.cps"
  override val description = "cps-transform plugin"

  def init(options: List[String]): List[PluginPhase] = {
     val shiftedSymbols = new ShiftedSymbols()
     val selectedNodes = new SelectedNodes()
     (new PhaseSelect(selectedNodes))::
       (new PhaseCps(selectedNodes,shiftedSymbols)) ::
       (new PhaseCpsAsyncShift(shiftedSymbols)) :: Nil
  }
}
