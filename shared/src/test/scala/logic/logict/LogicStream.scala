package logic.logict

import cps.CpsTryMonad


//
// TODO: (when find time)
//  implement version of LogiSeq with direct suspend (similar to haskell Control.Monad.Stream + LogicSeqT optimization)
//

sealed trait LogicStream[F[_]:CpsTryMonad,A] {

}

object LogicStream {
  
  case class Empty[F[_]:CpsTryMonad,A]() extends LogicStream[F,A]

  case class Pure[F[_]:CpsTryMonad,A](a:A) extends LogicStream[F,A]
  
  case class Error[F[_]:CpsTryMonad,A](e:Throwable) extends LogicStream[F,A]
  
  case class Cons[F[_]:CpsTryMonad,A](head:Try[A], tail: ()=>LogicStream[F,A]) extends LogicStream[F,A]
  
  case class WaitF[F[_]:CpsTryMonad,A](waited:F[LogicStream[F,A]]) extends LogicStream[F,A]
  
  case class Suspend[F[_]:CpsTryMonad,A](suspended: ()=>LogicStream[F,A]) extends LogicStream[F,A]
  
  
}
