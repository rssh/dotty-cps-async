package logic.logict

import cps.*

import scala.collection.immutable.Queue
import scala.util.*
import scala.util.control.NonFatal


//
// TODO: (when find time)
//  implement version of LogiSeq with direct suspend (similar to haskell Control.Monad.Stream + LogicSeqT optimization)
//

sealed trait LogicStream[F[_]:CpsTryMonad,A] {

  def map[B](f: A=>B):LogicStream[F,B]
  def flatMap[B](f: A=>LogicStream[F,B]):LogicStream[F,B]

  def flatMapTry[B](f: Try[A]=>LogicStream[F,B]):LogicStream[F,B]

  def mplus(other: =>LogicStream[F,A]):LogicStream[F,A]

  def fsplit: F[Option[(Try[A],LogicStream[F,A])]]
  def msplit: LogicStream[F,Option[(Try[A],LogicStream[F,A])]]

}

object LogicStream {

  case class Empty[F[_]:CpsTryMonad,A]() extends LogicStream[F,A] {

    override def map[B](f:A=>B):LogicStream[F,B] =
      Empty[F,B]()

    override def flatMap[B](f:A=>LogicStream[F,B]):LogicStream[F,B] =
      Empty[F,B]()

    override def flatMapTry[B](f:Try[A]=>LogicStream[F,B]):LogicStream[F,B] =
      Empty[F,B]()

    override def mplus(other: =>LogicStream[F, A]): LogicStream[F, A] =
      other

    override def fsplit:F[Option[(Try[A],LogicStream[F,A])]] =
      summon[CpsTryMonad[F]].pure(None)

    override def msplit: LogicStream[F,Option[(Try[A],LogicStream[F,A])]] =
      Empty[F,Option[(Try[A],LogicStream[F,A])]]()

  }

  case class Pure[F[_]:CpsTryMonad,A](a:A) extends LogicStream[F,A] {

    override def map[B](f:A=>B):LogicStream[F,B] =
      try
        Pure[F,B](f(a))
      catch
        case  NonFatal(ex) => Error[F,B](ex)

    override def flatMap[B](f:A=>LogicStream[F,B]):LogicStream[F,B] =
      try
        f(a)
      catch
        case  NonFatal(ex) => Error[F,B](ex)

    override def flatMapTry[B](f:Try[A]=>LogicStream[F,B]):LogicStream[F,B] =
      try
        f(Success(a))
      catch
        case NonFatal(ex) => Error[F,B](ex)

    override def mplus(other: => LogicStream[F, A]): LogicStream[F, A] =
      Cons(Success(a), ()=>other)

    override def fsplit: F[Option[(Try[A],LogicStream[F,A])]] =
      summon[CpsTryMonad[F]].pure(Some((Success(a),Empty[F,A]())))

    override def msplit: LogicStream[F, Option[(Try[A],LogicStream[F,A])]] =
      mpure(Some((Success(a),Empty[F,A]())))

  }

  case class Error[F[_]:CpsTryMonad,A](e:Throwable) extends LogicStream[F,A] {

    override def map[B](f:A=>B):LogicStream[F,B] =
      Error[F,B](e)

    override def flatMap[B](f:A=>LogicStream[F,B]):LogicStream[F,B] =
      Error[F,B](e)

    override def flatMapTry[B](f:Try[A]=>LogicStream[F,B]):LogicStream[F,B] =
      try {
        f(Failure(e))
      } catch {
        case NonFatal(ex) => Error[F,B](ex)
      }

    override def mplus(other: => LogicStream[F, A]): LogicStream[F, A] =
      Cons(Failure(e), ()=>other)

    override def fsplit: F[Option[(Try[A],LogicStream[F,A])]] =
      summon[CpsTryMonad[F]].pure(Some((Failure(e),Empty[F,A]())))

    override def msplit: LogicStream[F, Option[(Try[A],LogicStream[F,A])]] =
      mpure(None)

  }

  case class Cons[F[_]:CpsTryMonad,A](head:Try[A], tail: ()=>LogicStream[F,A]) extends LogicStream[F,A] {

    override def map[B](f:A=>B):LogicStream[F,B] =
      head match
        case Success(a) =>
          try
            Cons[F,B](Success(f(a)), ()=>tail().map(f))
          catch
            case NonFatal(ex) => Error[F,B](ex)
        case Failure(ex) =>
          Error[F,B](ex)

    override def flatMap[B](f:A=>LogicStream[F,B]):LogicStream[F,B] =
      head match
        case Success(a) =>
          try
            f(a).mplus(tail().flatMap(f))
          catch
            case NonFatal(ex) => Error[F,B](ex)
        case Failure(ex) =>
          Error[F,B](ex)

    override def flatMapTry[B](f:Try[A]=>LogicStream[F,B]):LogicStream[F,B] =
      try
        f(head).mplus(tail().flatMapTry(f))
      catch
        case NonFatal(ex) => Error[F,B](ex)

    override def mplus(other: => LogicStream[F, A]): LogicStream[F, A] =
      MPlusSeq[F,A](Queue(this,other))

    override def fsplit: F[Option[(Try[A],LogicStream[F,A])]] =
      summon[CpsTryMonad[F]].pure(Some((head,tail())))

    override def msplit: LogicStream[F, Option[(Try[A],LogicStream[F,A])]] =
      mpure(Some((head,tail())))

  }

  case class MPlusSeq[F[_]:CpsTryMonad,A](queue: Queue[LogicStream[F,A]]) extends LogicStream[F,A] {

    override def map[B](f:A=>B):LogicStream[F,B] =
      MPlusSeq[F,B](queue.map(_.map(f)))

    override def flatMap[B](f:A=>LogicStream[F,B]):LogicStream[F,B] =
      MPlusSeq[F,B](queue.map(_.flatMap(f)))

    override def flatMapTry[B](f:Try[A]=>LogicStream[F,B]):LogicStream[F,B] =
      MPlusSeq[F,B](queue.map(_.flatMapTry(f)))

    override def mplus(other: => LogicStream[F, A]): LogicStream[F, A] =
      MPlusSeq[F,A](queue.enqueue(Suspend(() => other)))

    override def fsplit: F[Option[(Try[A],LogicStream[F,A])]] =
      queue.dequeueOption match
        case None =>
          summon[CpsTryMonad[F]].pure(None)
        case Some((head,tail)) =>
          summon[CpsTryMonad[F]].flatMap(head.fsplit){
            case None =>
              MPlusSeq[F,A](tail).fsplit
            case Some((headHead,tailHead)) =>
              if (tail.isEmpty) then
                summon[CpsTryMonad[F]].pure(Some((headHead,tailHead)))
              else
                summon[CpsTryMonad[F]].pure(Some((headHead, tailHead.mplus(MPlusSeq(tail)))))
          }


    override def msplit: LogicStream[F, Option[(Try[A],LogicStream[F,A])]] =
      WaitF(
        summon[CpsTryMonad[F]].map(fsplit){
          case None => Empty[F,Option[(Try[A],LogicStream[F,A])]]()
          case Some((head,tail)) =>
            mpure(Some((head,tail)))
        }
      )

  }

  case class WaitF[F[_]:CpsTryMonad,A](waited:F[LogicStream[F,A]]) extends LogicStream[F,A] {

    override def map[B](f:A=>B):LogicStream[F,B] =
      WaitF[F,B](summon[CpsTryMonad[F]].map(waited)(_.map(f)))

    override def flatMap[B](f:A=>LogicStream[F,B]):LogicStream[F,B] =
      WaitF[F,B](summon[CpsTryMonad[F]].map(waited)(_.flatMap(f)))

    override def flatMapTry[B](f:Try[A]=>LogicStream[F,B]):LogicStream[F,B] =
      WaitF[F,B](summon[CpsTryMonad[F]].map(waited)(_.flatMapTry(f)))

    override def mplus(other: => LogicStream[F, A]): LogicStream[F, A] =
      MPlusSeq[F,A](Queue(this,Suspend(() => other)))

    override def fsplit: F[Option[(Try[A],LogicStream[F,A])]] =
      summon[CpsTryMonad[F]].flatMap(waited)(_.fsplit)

    override def msplit: LogicStream[F, Option[(Try[A],LogicStream[F,A])]] =
      WaitF[F,Option[(Try[A],LogicStream[F,A])]](
        summon[CpsTryMonad[F]].map(waited)(_.msplit)
      )

  }

  case class Suspend[F[_]:CpsTryMonad,A](suspended: ()=>LogicStream[F,A]) extends LogicStream[F,A] {

    override def map[B](f:A=>B):LogicStream[F,B] =
      Suspend[F,B](()=>suspended().map(f))

    override def flatMap[B](f:A=>LogicStream[F,B]):LogicStream[F,B] =
      Suspend[F,B](()=>suspended().flatMap(f))

    override def flatMapTry[B](f:Try[A]=>LogicStream[F,B]):LogicStream[F,B] =
      Suspend[F,B](()=>suspended().flatMapTry(f))

    override def mplus(other: => LogicStream[F, A]): LogicStream[F, A] =
       Suspend[F,A](()=>suspended().mplus(other))

    override def fsplit: F[Option[(Try[A],LogicStream[F,A])]] =
      suspended().fsplit

    override def msplit: LogicStream[F, Option[(Try[A],LogicStream[F,A])]] =
      suspended().msplit
  }


  def mpure[F[_]:CpsTryMonad, A](a:A): LogicStream[F,A] =
    Pure[F,A](a)

}
