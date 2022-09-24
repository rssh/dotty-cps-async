package cps.plugin


import dotty.tools.dotc.*
import core.*
import core.Contexts.*
import ast.tpd.*
import plugins.*
import transform.PruneErasedDefs


class PhaseCpsAsyncShift(shiftedSymbols:ShiftedSymbols) extends PluginPhase {

  val phaseName = "cpsAsyncShift"

  override val runsAfter  = Set("cps")
  override val runsBefore = Set(PruneErasedDefs.name)

  override def run(using Context): Unit = {
    // TODO:
    //  check what async-shift needed in the current compilation unit
    //   -- generate one in the special class
    //   -- update the global cache, setting as key - signature of function or method, value - async tree
  }

}

