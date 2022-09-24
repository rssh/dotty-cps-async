package cps.plugin


import dotty.tools.dotc.*
import core.*
import core.Contexts.*
import ast.tpd.*
import plugins.*


class PhaseCps(shiftedSymbols:ShiftedSymbols) extends PluginPhase {

  val phaseName = "cps"

  override val runsAfter = Set("cc")
  override val runsBefore = Set("cpsAsyncShift")

  override def transformDefDef(tree:DefDef)(using Context): Tree = {
    // TODO:
    //  find parameter with capacity CpsTransform[M] and outer capacity with this parameter
    //  Translate to function which return M[T] instead T which body is cps-transformed
    tree
  }

}

