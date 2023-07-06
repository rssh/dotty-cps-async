package cps.plugin.forest

import dotty.tools.dotc.*
import core.Contexts.*
import util.{NoSourcePosition, SrcPos}
import cps.plugin.CpsTopLevelContext

object Log {

  def apply(logLevel: Int, message: String, nesting: Int, srcPos: SrcPos=NoSourcePosition)(using Context, CpsTopLevelContext): Unit = {
      if (summon[CpsTopLevelContext].debugSettings.debugLevel >= logLevel) {
         val shiftedMessage = ""+nesting+" "*(nesting*3)  + message
         println(shiftedMessage)
         report.log(shiftedMessage, srcPos)
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
