package cps.plugin

import dotty.tools.dotc.*
import ast.tpd.*
import core.*
import core.Symbols.*
import core.Types.*

class ShiftedSymbols {

  private val storage: MutableSymbolMap[AsyncShiftRecord] = new MutableSymbolMap()

  def translateShiftedCall(sym: Symbol): Symbol = {
    // TODO: create symbol of the same type with the same owner
    ??? 
  }

  // called from phase cpsAsyncShift
  def checkUsage(origin: Symbol): Option[AsyncShiftRecord] = {
    storage.get(origin)
  }


}

case class AsyncShiftRecord(originSymbol: Symbol, shiftedSymbol: Symbol, monads: Set[Type])


