package cps.plugin

import dotty.tools.dotc.*
import ast.tpd.*
import core.*
import core.Symbols.*
import core.Types.*

class ShiftedSymbols {

  private val storage: MutableSymbolMap[AsyncShiftRecord] = new MutableSymbolMap()

  def addAsyncShift(originSym: Symbol, shiftedMethod: DefDef): Unit =
    storage.get(originSym) match
      case Some(r) =>
        throw IllegalStateException(s"DefDef already exists: ${originSym}")
      case None =>
        storage.update(originSym, AsyncShiftRecord(originSym, shiftedMethod))

  def translateShiftedCall(sym: Symbol): Symbol =
    // TODO: create symbol of the same type with the same owner
    ???

  // called from phase cpsAsyncShift
  def getRecord(origin: Symbol): Option[AsyncShiftRecord] =
    storage.get(origin)

}

class AsyncShiftRecord(val originSymbol: Symbol, val shiftedMethod: DefDef)
