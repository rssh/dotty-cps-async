package cps.chessboard

enum Color:
  case Black, White

enum Figure:
  case Pawn, Knight, Bishop, Rook, Queen, King

case class Chessman(figure: Figure, color: Color)

opaque type Chessboard = Map[(Int,Int),Chessman]

object Chessboard:
   def empty = Map.empty
   val size = 8

   extension (board: Chessboard)

       def put(x:Int, y:Int, chessman: Chessman): Option[Chessboard] =
         require(board.isInside(x,y))
         val xy = (x,y)
         board.get(xy) match
           case Some(c) => None
           case None => Some(board.updated(xy,chessman))


       def check(x:Int, y:Int): Option[Chessman] =
         board.get((x,y))

       def remove(x:Int, y:Int): Chessboard =
         board.removed((x,y))

       def isInside(x:Int, y:Int): Boolean =
         x >= 0 && x < Chessboard.size &&
         y >= 0 && y < Chessboard.size

       def color(x:Int, y:Int): Color =
         if ((x+y) % 2 == 0)
           Color.Black
         else
           Color.White

       def isFull: Boolean =
         (0 until size).forall(x =>
           (0 until size).forall(y =>
             check(x,y).isDefined
                              ) )

