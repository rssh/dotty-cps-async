package cps.chessboard

import cps._
import cps.testconfig.given

//TODO: move to cps.runtime

given CpsMonad[List] with CpsMonadInstanceContext[List] with

   def pure[T](t:T):List[T] =
        List(t)

   def map[A,B](fa:List[A])(f: A=>B):List[B]=
        fa map f

   def flatMap[A,B](fa:List[A])(f: A=>List[B]):List[B]=
        fa flatMap f




object Tour {

   case class Point(x:Int, y:Int)

   case class State(
     currentPoint: Point,
     board: Chessboard,
     trace: List[Point]
   )

   val knight = Chessman(Figure.Knight,Color.White)

   def nextMoves(state: State):List[State] = 
        def inStep(sx:Int,sy:Int):List[State] =
           for
              dx <- List(-sx,sx) 
              x = state.currentPoint.x + dx  
              if (x >= 0 && x < Chessboard.size)
              dy <- List(-sy,sy)
              y = state.currentPoint.y + dy  
              if (y >= 0 && y < Chessboard.size)
              nextBoard <- state.board.put(x,y,knight).toList
           yield
              state.copy(
                board = nextBoard,
                trace = Point(x,y)::state.trace 
              )
        inStep(2,1) ++ inStep(1,2)
       

   def findPath(initPos:Point, state: State): List[State] = 
       async[List]{
          val nextState = await(nextMoves(state))
          if (state.board.isFull)
            if (initPos == nextState.currentPoint)
               nextState
            else
               await(Nil)
          else
            await(findPath(initPos,nextState))
       }
   
}
