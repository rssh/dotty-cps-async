package cps.logic


trait Game01Tree[Position, Move] {

  def isFinalPosition(pos: Position): Option[Game01Tree.GameOutcome]

  def nextMoves(pos: Position, side: Boolean): Seq[Move]

  def applyMove(pos: Position, move: Move, side: Boolean): Position

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


  trait Gamer[F[_], Position, Move] {

    def makeMove(pos: Position, side: Boolean): F[Move]

  }

}

