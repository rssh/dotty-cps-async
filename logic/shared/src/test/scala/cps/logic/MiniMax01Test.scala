package cps.logic

import cps.*
import cps.logic.Game01Tree.GameOutcome
import cps.logic.Game01Tree.Goal.{NoLose, Win}
import cps.monads.logic.{*, given}
import org.junit.{Assert, Test}

trait Game01Tree[Position, Move] {

  def isFinalPosition(pos:Position): Option[Game01Tree.GameOutcome]

  def nextMoves(pos:Position, side: Boolean): Seq[Move]

  def applyMove(pos:Position, move: Move, side: Boolean): Position

}

object Game01Tree {

 sealed trait GameOutcome

 object GameOutcome:
    case class Win(side: Boolean) extends GameOutcome
    case object Draw extends GameOutcome

 sealed trait MoveOutcome[Move]

 object MoveOutcome:
   case class Win[Move](side: Boolean, move:Move) extends MoveOutcome[Move]
   case class Lose[Move](side: Boolean, move: Move) extends MoveOutcome[Move]
   case class NotFinalOrDraw[Move](side: Boolean, move: Move) extends MoveOutcome[Move]

 enum Goal:
    case Win, NoLose

 def goalOfOpposite(goal: Goal): Goal = goal match
   case Goal.Win => Goal.NoLose
   case Goal.NoLose => Goal.Win

}

object MiniMax01 {

  import Game01Tree.*

  val MAX_DEPTH = 20



  def goalOutcomes[Position, Move](game: Game01Tree[Position,Move],
                                      pos: Position,
                                      side: Boolean,
                                      goal: Goal,
                                      depth: Int): LogicStream[MoveOutcome[Move]] = reify[LogicStream]{
     val nextMove = choicesFrom(game.nextMoves(pos, side))
     //val depthPrefix = s"${" "*depth}(${depth}) side ${sideState(side)} goal $goal "
     //println(s"${depthPrefix}checking move ${nextMove}  pos ${pos} depth ${depth} ")
     val nextPos = game.applyMove(pos, nextMove, side)
     game.isFinalPosition(nextPos) match
       case Some(x) =>
         x match
           case GameOutcome.Win(x) =>
              if (x == side)
                then MoveOutcome.Win(side,nextMove)
                else choices.empty[MoveOutcome[Move]]
           case GameOutcome.Draw =>
              if (goal == Goal.Win)
                then choices.empty[MoveOutcome[Move]]
                else MoveOutcome.NotFinalOrDraw(side,nextMove)
       case None =>
         val otherOutcomes = once(goalOutcomes(game, nextPos, !side, goalOfOpposite(goal), depth + 1))
         val myOutcomes = otherOutcomes.ifThenElseM{ yOutcome =>
            //println(s"${depthPrefix}find outcome $yOutcome for other side on pos $nextPos, after my move ${nextMove} no target outcome for me")
            yOutcome match
              case MoveOutcome.Win(otherSide, yMove) => LogicStream.empty[MoveOutcome[Move]]
              case MoveOutcome.Lose(otherSide, yMove) => LogicStream.pure(MoveOutcome.Win(side,nextMove)) // never happes
              case MoveOutcome.NotFinalOrDraw(otherSide,yMove) =>
                goal match
                  case Goal.Win => LogicStream.empty[MoveOutcome[Move]]
                  case Goal.NoLose => LogicStream.pure(MoveOutcome.NotFinalOrDraw(side,nextMove))
         } {
           //println(s"${depthPrefix} on pos $nextPos, noOther outcomes, declare goal ${goal} for move ${nextMove}")
           goal match
             case Goal.Win => LogicStream.pure(MoveOutcome.Win(side,nextMove))
             case Goal.NoLose => LogicStream.pure(MoveOutcome.NotFinalOrDraw(side,nextMove))
         }
         reflect(myOutcomes)
  }


}



enum CellState:
  case X, O, Empty

def sideState(side: Boolean): CellState = if side then CellState.X else CellState.O

case class Point(x:Int, y:Int)

case class TicTacToeBoardN(cells:Seq[Seq[CellState]]) {
  def size: Int = cells.size
}

object TicTacGame extends Game01Tree[TicTacToeBoardN, Point] {

  def init(size: Int): TicTacToeBoardN = TicTacToeBoardN(Seq.fill(size, size)(CellState.Empty))

  def isFinalPosition(pos: TicTacToeBoardN): Option[GameOutcome] = {
    if (isWinningPosition(pos, true)) {
      Some(GameOutcome.Win(true))
    } else if (isWinningPosition(pos, false)) {
      Some(GameOutcome.Win(false))
    } else if (pos.cells.forall(_.forall(_ != CellState.Empty))) {
      Some(GameOutcome.Draw)
    } else {
      None
    }
  }

  def isWinningPosition(pos:TicTacToeBoardN, currentSide: Boolean):Boolean = {
    val sideCell = sideState(currentSide)
    val win = (0 until pos.size).exists(x => (0 until pos.size).forall(y => pos.cells(x)(y) == sideCell)) ||
      (0 until pos.size).exists(y => (0 until pos.size).forall(x => pos.cells(y)(x) == sideCell) ||
        (0 until pos.size).forall(x => pos.cells(x)(x) == sideCell) ||
        (0 until pos.size).forall(x => pos.cells(x)(pos.size - x - 1) == sideCell))
    //if (win) {
    //  println(s"isWinnign: ${win} for side ${currentSide} pos $pos")
    //}
    win
  }

  def nextMoves(pos:TicTacToeBoardN, side:Boolean): Seq[Point] = {
    for(x <- 0 until pos.size; y <- 0 until pos.size; if pos.cells(x)(y) == CellState.Empty) yield Point(x,y)
  }

  def applyMove(pos:TicTacToeBoardN, move: Point, side: Boolean): TicTacToeBoardN = {
    val newCells = pos.cells.updated(move.x, pos.cells(move.x).updated(move.y, sideState(side)))
    TicTacToeBoardN(newCells)
  }

}





class MiniMax01Test {

  @Test
  def testFindWinningMoveTicTacToe3x3Win() = {
    val game = TicTacGame
    val board = TicTacGame.init(3)
    val winningMove = once(MiniMax01.goalOutcomes(game, board, true, Win, 0)).toLazyList
    //println("winnitnMove: "+winningMove.headOption)
    assert(winningMove.isEmpty)
  }

  @Test
  def testFindWinningMoveTicTacToe3x3NoLose() = {
    val game = TicTacGame
    val board = TicTacGame.init(3)
    val noLoseMove = once(MiniMax01.goalOutcomes(game, board, true, NoLose, 0)).toLazyList
    //println("noLoseMove: "+noLoseMove.headOption)
    assert(noLoseMove.nonEmpty)
  }



}
