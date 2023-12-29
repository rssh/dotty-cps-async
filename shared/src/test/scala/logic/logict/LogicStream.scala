package logic.logict


//
// TODO: (when find time)
//  implement version of LogiSeq with direct suspend (similar to haskell Control.Monad.Stream + LogicSeqT optimization)
//

sealed trait LogicMixStream[F[_],+A] {

}

object LogicMixStream {
  case object Empty extends LogicMixStream[?,Nothing]

}
