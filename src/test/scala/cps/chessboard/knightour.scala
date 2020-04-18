package cps.chessboard

import cps._

//TODO: move to cps.runtime

given CpsMonad[List]:

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

   def nextMoves(state: State):List[State] = ???
       

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
