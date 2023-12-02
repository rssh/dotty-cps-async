package logic.slm

import cps.*
import cps.monads.{*, given}
import logic.slm.Deductive

import scala.concurrent.*
import scala.concurrent.ExecutionContext.Implicits.global



class TestQuins {

  def isBusy(quens: IndexedSeq[(Int,Int)], x: Int, y: Int): Boolean = {
    quens.exists { case (x1, y1) => x1 == x || y1 == y || (x1 - x).abs == (y1 - y).abs }
  }


  def solver(quens: IndexedSeq[(Int,Int)], n: Int, maxN:Int): Deductive[Future,IndexedSeq[(Int,Int)]] = async[[X]=>>Deductive[Future,X]] {
    if (n == 0) then
      quens
    else
      ???
  }

}
