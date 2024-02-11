package cps.logic

import cps.*
import cps.monads.logic.{*,given}

trait Game01Tree[Position] {

  def isWinningPosition(pos:Position):Boolean

  def nextMoves(pos:Position): Seq[Position]

}

object Game01Tree {

 sealed trait Outcome[Position]

 object Outcome:
   case class Win[Position](path:Seq[Position]) extends Outcome[Position]
   case class Lose[Position](path:Seq[Position]) extends Outcome[Position]
   case class NotFinal[Position](path:Seq[Position]) extends Outcome[Position]
}

object MiniMax01 {

  import Game01Tree.*


  def isWinning[Position](tree:Game01Tree[Position], pos: Position, side: Boolean, depth: Int): Boolean = {
     once(reachableNoLoseOutcomes(tree, pos, side, depth)).toLazyList.exists {
       case Outcome.Win(_) => true
       case _ => false
     }
  }

  def reachableNoLoseOutcomes[Position](tree:Game01Tree[Position], pos: Position, side: Boolean, maxDepth: Int): LogicStream[Outcome[Position]] = reify[LogicStream] {
     if (tree.isWinningPosition(pos)) {
       Outcome.Win(Seq(pos))
     } else if (maxDepth <= 0) {
       Outcome.NotFinal(Seq(pos))
     } else {
       val next = choicesFrom(tree.nextMoves(pos))
       val yOutcome = reflect(reachableNoLoseOutcomes(tree, next, !side, maxDepth-1))
       val xOutcome = yOutcome match
         case Outcome.Win(_) => LogicStream.noChoices[Outcome[Position]]
         case Outcome.Lose(path) => Outcome.Win(pos +: path)
         case Outcome.NotFinal(path) => Outcome.NotFinal(pos +: path)
       xOutcome
     }
  }

}

class MiniMax01Test {


}
