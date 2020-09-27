package cps.chessboard

object ChessSymbol:

  def apply(figure: Figure, color: Color): Char = 
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
       

