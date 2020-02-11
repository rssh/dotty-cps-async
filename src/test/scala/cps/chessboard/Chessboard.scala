package cps.chessboard

enum Color:
  case Black, White

enum Figure:
  case Pawn, Knight, Bishop, Rook, Queen, King

case class Chessman(figure: Figure, color: Color):
  def symbol: Char = 
    color match
      case Color.Black =>
        figure match 
          case Figure.Pawn => '\u265F'
          case Figure.Knight => '\u265E'
          case Figure.Bishop => '\u265D'
          case Figure.Rook => '\u265C'
          case Figure.Queen => '\u265B'
          case Figure.King => '\u265A'
      case Color.White =>
        figure match 
          case Figure.Pawn => '\u2659'
          case Figure.Knight => '\u2658'
          case Figure.Bishop => '\u2657'
          case Figure.Rook => '\u2656'
          case Figure.Queen => '\u2655'
          case Figure.King => '\u2654'
       

opaque type Chessboard = Map[(Int,Int),Chessman]

object Chessboard:
   def empty = Map.empty
   val size = 8

/*
 TODO: enable after dorry-0.22 release

   extension chessboardOps on (board: Chessboard)

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
      
*/
