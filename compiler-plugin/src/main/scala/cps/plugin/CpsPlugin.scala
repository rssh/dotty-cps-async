/**
 * Cps compiler plugin.
 **/
package cps.plugin

import dotty.tools.dotc.*
import dotty.tools.dotc.plugins.*

class CpsPlugin extends StandardPlugin {

  override val name        = "rssh.cps"
  override val description = "cps-transform plugin"

  //
  //  def a(){
  //     def b() { .. }
  //     def c() { .. }
  //  }
  //
  

  override def init(options: List[String]): List[PluginPhase] = {
     val settings = parseOptions(options)
     val selectedNodes = new SelectedNodes()
     List(
       new PhaseSelectAndGenerateShiftedMethods(selectedNodes),
       new PhaseCps(settings,selectedNodes),
       new PhaseChangeSymbolsAndRemoveScaffolding(selectedNodes)
     )
  }

  private def parseOptions(options:List[String]): CpsPluginSettings = {
    val settings = new CpsPluginSettings()
    for (option <- options) {
      if (option.startsWith("debugLevel=")) {
        val level = option.substring("debugLevel=".length).toInt
        settings.debugLevel = level
      } else if (option == "useLoom") {
        settings.useLoom = true
      } else {
        throw new IllegalArgumentException(s"Unknown option for cps plugin: $option")
      }
    }
    settings
  }

}
