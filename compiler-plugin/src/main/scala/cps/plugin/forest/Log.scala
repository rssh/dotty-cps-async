package cps.plugin.forest

import dotty.tools.dotc.*
import dotty.tools.dotc.core.Decorators.containsPhase
import core.Contexts.*
import util.{NoSourcePosition, SrcPos}
import cps.plugin.CpsTopLevelContext

object Log {

  val regardlessYLog = true
  var noYLogWasWarned = false

  def apply(logLevel: Int, message: String, nesting: Int, srcPos: SrcPos=NoSourcePosition)(using Context, CpsTopLevelContext): Unit = {
      if (summon[CpsTopLevelContext].debugSettings.debugLevel >= logLevel) {
         val shiftedMessage = ""+nesting+" "*(nesting*3)  + message
         if (ctx.settings.Ylog.value.containsPhase(ctx.phase)) then
            report.log(shiftedMessage, srcPos)
         else
            if (!noYLogWasWarned) then
                report.warning("Ylog is disabled, to enable it, use -Ylog:<phase>", srcPos)
                noYLogWasWarned = true
            if (regardlessYLog) then
                println(shiftedMessage)
      }
  }

  def info(message: String, nesting: Int, srcPos: SrcPos = NoSourcePosition)(using Context, CpsTopLevelContext): Unit = {
    apply(0, message, nesting, srcPos)
  }

  def debug(message: String, nesting: Int, srcPos: SrcPos = NoSourcePosition)(using Context, CpsTopLevelContext): Unit = {
      apply(5, message, nesting, srcPos)
  }

  def trace(message: String, nesting: Int, srcPos: SrcPos = NoSourcePosition)(using Context, CpsTopLevelContext): Unit = {
    apply(10, message, nesting, srcPos)
  }




}
