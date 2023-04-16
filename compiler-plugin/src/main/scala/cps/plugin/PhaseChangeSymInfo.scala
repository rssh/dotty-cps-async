package cps.plugin

import dotty.tools.dotc.plugins.PluginPhase

import scala.annotation.*
import dotty.tools.dotc.*
import core.*
import core.Decorators.*
import core.Contexts.*
import core.Names.*
import core.Symbols.*
import core.Types.*
import ast.tpd.*
import plugins.*
import cps.plugin.QuoteLikeAPI.*
import cps.plugin.forest.*
import dotty.tools.dotc.ast.{Trees, tpd}
import dotty.tools.dotc.core.DenotTransformers.InfoTransformer
import dotty.tools.dotc.util.SrcPos

class PhaseChangeSymInfo(settings: CpsPluginSettings, selectedNodes: SelectedNodes) extends PluginPhase with InfoTransformer {

  val phaseName = "rssh.cpsChangeSymInfo"

  override val runsAfter = Set(PhaseSelect.phaseName)
  override val runsBefore = Set("rssh.cps")


  override def transformInfo(tp: Type, sym: Symbol)(using Context): Type = {
    //println(s"transformInfo: ${tp.show} ${sym.showFullName}, ${sym.id}, isOur=${selectedNodes.getDefDefRecord(sym).isDefined}")
    if (sym.isClass) {
      // val here = sym.
    }
    selectedNodes.getDefDefRecord(sym) match
      case None => tp
      case Some(selectRecord) =>
        val cpsMonadContext = selectRecord.kind.getCpsMonadContext
        val monadType = CpsTransformHelper.extractMonadType(cpsMonadContext.tpe, sym.srcPos)
        // TODO: check that cpsTransformedType works correctly with method type
        var debug = false
        val ntp = CpsTransformHelper.cpsTransformedType(tp, monadType, debug)
        selectRecord.changedType = ntp
        println(s"transformInfo: ${tp.show}  ->  ${ntp.show}, ${sym} '${sym.name.mangledString}' ${sym.id}")
        ntp
  }

  override def infoMayChange(sym: Symbol)(using Context): Boolean = {
    selectedNodes.getDefDefRecord(sym).isDefined
  }

  override def transform(ref: Denotations.SingleDenotation)(using Context): Denotations.SingleDenotation = {
    if (ref.symbol.exists && infoMayChange(ref.symbol)) {
      println("transform denotaion: " + ref.symbol.debugString)
      super.transform(ref)
    } else {
      ref
    }
  }

}
