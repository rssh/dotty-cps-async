package logic.logict

import cps.*
import cps.monads.{*, given}
import logic.{CpsLogicMonad, CpsLogicMonadInstanceContext}

import scala.collection.immutable.Queue
import scala.util.*
import scala.util.control.NonFatal


//
//  Version of LogiSeq with direct suspend (similar to haskell Control.Monad.Stream + LogicSeqT optimization)
//


type LogicStream[A] = LogicStreamT[CpsIdentity,A]

object LogicStream {

  def apply[A]: LogicStream[A] =
    LogicStreamT.Empty[CpsIdentity,A]()

}

sealed trait LogicStreamT[F[_]:CpsTryMonad,A] {

  def map[B](f: A=>B):LogicStreamT[F,B]
  def flatMap[B](f: A=>LogicStreamT[F,B]):LogicStreamT[F,B]

  def flatMapTry[B](f: Try[A]=>LogicStreamT[F,B]):LogicStreamT[F,B]

  def mplus(other: =>LogicStreamT[F,A]):LogicStreamT[F,A]

  def fsplit: F[Option[(Try[A],LogicStreamT[F,A])]]
  def msplit: LogicStreamT[F,Option[(Try[A],LogicStreamT[F,A])]]
  
  
}

object LogicStreamT {

  case class Empty[F[_]:CpsTryMonad,A]() extends LogicStreamT[F,A] {

    override def map[B](f:A=>B):LogicStreamT[F,B] =
      Empty[F,B]()

    override def flatMap[B](f:A=>LogicStreamT[F,B]):LogicStreamT[F,B] =
      Empty[F,B]()

    override def flatMapTry[B](f:Try[A]=>LogicStreamT[F,B]):LogicStreamT[F,B] =
      Empty[F,B]()

    override def mplus(other: =>LogicStreamT[F, A]): LogicStreamT[F, A] =
      other

    override def fsplit:F[Option[(Try[A],LogicStreamT[F,A])]] =
      summon[CpsTryMonad[F]].pure(None)

    override def msplit: LogicStreamT[F,Option[(Try[A],LogicStreamT[F,A])]] =
      mpure(None)

  }

  case class Pure[F[_]:CpsTryMonad,A](a:A) extends LogicStreamT[F,A] {

    override def map[B](f:A=>B):LogicStreamT[F,B] =
      try
        Pure[F,B](f(a))
      catch
        case  NonFatal(ex) => Error[F,B](ex)

    override def flatMap[B](f:A=>LogicStreamT[F,B]):LogicStreamT[F,B] =
      try
        f(a)
      catch
        case  NonFatal(ex) => Error[F,B](ex)

    override def flatMapTry[B](f:Try[A]=>LogicStreamT[F,B]):LogicStreamT[F,B] =
      try
        f(Success(a))
      catch
        case NonFatal(ex) => Error[F,B](ex)

    override def mplus(other: => LogicStreamT[F, A]): LogicStreamT[F, A] =
      Cons(Success(a), ()=>other)

    override def fsplit: F[Option[(Try[A],LogicStreamT[F,A])]] =
      summon[CpsTryMonad[F]].pure(Some((Success(a),Empty[F,A]())))

    override def msplit: LogicStreamT[F, Option[(Try[A],LogicStreamT[F,A])]] =
      mpure(Some((Success(a),Empty[F,A]())))

  }

  case class Error[F[_]:CpsTryMonad,A](e:Throwable) extends LogicStreamT[F,A] {

    override def map[B](f:A=>B):LogicStreamT[F,B] =
      Error[F,B](e)

    override def flatMap[B](f:A=>LogicStreamT[F,B]):LogicStreamT[F,B] =
      Error[F,B](e)

    override def flatMapTry[B](f:Try[A]=>LogicStreamT[F,B]):LogicStreamT[F,B] =
      try {
        f(Failure(e))
      } catch {
        case NonFatal(ex) => Error[F,B](ex)
      }

    override def mplus(other: => LogicStreamT[F, A]): LogicStreamT[F, A] =
      Cons(Failure(e), ()=>other)

    override def fsplit: F[Option[(Try[A],LogicStreamT[F,A])]] =
      summon[CpsTryMonad[F]].pure(Some((Failure(e),Empty[F,A]())))

    override def msplit: LogicStreamT[F, Option[(Try[A],LogicStreamT[F,A])]] =
      mpure(Some((Failure(e),Empty[F,A]())))

  }

  case class Cons[F[_]:CpsTryMonad,A](head:Try[A], tail: ()=>LogicStreamT[F,A]) extends LogicStreamT[F,A] {

    //println(s"create Cons($head, $tail)")

    override def map[B](f:A=>B):LogicStreamT[F,B] =
      head match
        case Success(a) =>
          try
            Cons[F,B](Success(f(a)), ()=>tail().map(f))
          catch
            case NonFatal(ex) => Error[F,B](ex)
        case Failure(ex) =>
          Error[F,B](ex)

    override def flatMap[B](f:A=>LogicStreamT[F,B]):LogicStreamT[F,B] =
      head match
        case Success(a) =>
          try
            f(a).mplus(tail().flatMap(f))
          catch
            case NonFatal(ex) => Error[F,B](ex)
        case Failure(ex) =>
          Error[F,B](ex)

    override def flatMapTry[B](f:Try[A]=>LogicStreamT[F,B]):LogicStreamT[F,B] =
      try
        f(head).mplus(tail().flatMapTry(f))
      catch
        case NonFatal(ex) => Error[F,B](ex)

    override def mplus(other: => LogicStreamT[F, A]): LogicStreamT[F, A] =
      MPlusSeq[F,A](Queue(this,other))

    override def fsplit: F[Option[(Try[A],LogicStreamT[F,A])]] =
      summon[CpsTryMonad[F]].pure(Some((head,tail())))

    override def msplit: LogicStreamT[F, Option[(Try[A],LogicStreamT[F,A])]] =
      mpure(Some((head,tail())))

  }

  case class MPlusSeq[F[_]:CpsTryMonad,A](queue: Queue[LogicStreamT[F,A]]) extends LogicStreamT[F,A] {

    override def map[B](f:A=>B):LogicStreamT[F,B] =
      MPlusSeq[F,B](queue.map(_.map(f)))

    override def flatMap[B](f:A=>LogicStreamT[F,B]):LogicStreamT[F,B] =
      MPlusSeq[F,B](queue.map(_.flatMap(f)))

    override def flatMapTry[B](f:Try[A]=>LogicStreamT[F,B]):LogicStreamT[F,B] =
      MPlusSeq[F,B](queue.map(_.flatMapTry(f)))

    override def mplus(other: => LogicStreamT[F, A]): LogicStreamT[F, A] =
      MPlusSeq[F,A](queue.enqueue(Suspend(() => other)))

    override def fsplit: F[Option[(Try[A],LogicStreamT[F,A])]] =
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


    override def msplit: LogicStreamT[F, Option[(Try[A],LogicStreamT[F,A])]] =
      println("msplit, this=$this")
      queue.dequeueOption match
        case None =>
          Empty[F,Option[(Try[A],LogicStreamT[F,A])]]()
        case Some((head,tail)) =>
          head.msplit.mplus(MPlusSeq(tail).msplit)




  }

  case class WaitF[F[_]:CpsTryMonad,A](waited:F[LogicStreamT[F,A]]) extends LogicStreamT[F,A] {

    override def map[B](f:A=>B):LogicStreamT[F,B] =
      WaitF[F,B](summon[CpsTryMonad[F]].map(waited)(_.map(f)))

    override def flatMap[B](f:A=>LogicStreamT[F,B]):LogicStreamT[F,B] =
      WaitF[F,B](summon[CpsTryMonad[F]].map(waited)(_.flatMap(f)))

    override def flatMapTry[B](f:Try[A]=>LogicStreamT[F,B]):LogicStreamT[F,B] =
      WaitF[F,B](summon[CpsTryMonad[F]].map(waited)(_.flatMapTry(f)))

    override def mplus(other: => LogicStreamT[F, A]): LogicStreamT[F, A] =
      MPlusSeq[F,A](Queue(this,Suspend(() => other)))

    override def fsplit: F[Option[(Try[A],LogicStreamT[F,A])]] =
      summon[CpsTryMonad[F]].flatMap(waited)(_.fsplit)

    override def msplit: LogicStreamT[F, Option[(Try[A],LogicStreamT[F,A])]] =
      WaitF[F,Option[(Try[A],LogicStreamT[F,A])]](
        summon[CpsTryMonad[F]].map(waited)(_.msplit)
      )

  }

  case class Suspend[F[_]:CpsTryMonad,A](suspended: ()=>LogicStreamT[F,A]) extends LogicStreamT[F,A] {

    override def map[B](f:A=>B):LogicStreamT[F,B] =
      Suspend[F,B](()=>suspended().map(f))

    override def flatMap[B](f:A=>LogicStreamT[F,B]):LogicStreamT[F,B] =
      Suspend[F,B](()=>suspended().flatMap(f))

    override def flatMapTry[B](f:Try[A]=>LogicStreamT[F,B]):LogicStreamT[F,B] =
      Suspend[F,B](()=>suspended().flatMapTry(f))

    override def mplus(other: => LogicStreamT[F, A]): LogicStreamT[F, A] =
       Suspend[F,A](()=>suspended().mplus(other))

    override def fsplit: F[Option[(Try[A],LogicStreamT[F,A])]] =
      suspended().fsplit

    override def msplit: LogicStreamT[F, Option[(Try[A],LogicStreamT[F,A])]] =
      suspended().msplit
  }


  def mpure[F[_]:CpsTryMonad, A](a:A): LogicStreamT[F,A] =
    Pure[F,A](a)


  given [F[_]:CpsTryMonad]: CpsLogicMonad[[A]=>>LogicStreamT[F,A]] with CpsLogicMonadInstanceContext[[A]=>>LogicStreamT[F,A]] with {

    override type Observer[A] = F[A]

    override def observerCpsMonad: CpsTryMonad[F] = summon[CpsTryMonad[F]]

    override def pure[T](t: T): LogicStreamT[F, T] = mpure(t)(using observerCpsMonad)

    override def map[A, B](fa: LogicStreamT[F, A])(f: A => B): LogicStreamT[F, B] =
      fa.map(f)

    override def flatMap[A, B](fa: LogicStreamT[F, A])(f: A => LogicStreamT[F, B]): LogicStreamT[F, B] =
      fa.flatMap(f)

    override def flatMapTry[A, B](fa: LogicStreamT[F, A])(f: Try[A] => LogicStreamT[F, B]): LogicStreamT[F, B] =
      fa.flatMapTry(f)

    override def error[A](e: Throwable): LogicStreamT[F, A] =
      WaitF(observerCpsMonad.error(e))(using observerCpsMonad)

    override def mzero[A]: LogicStreamT[F, A] = Empty[F,A]()

    override def mplus[A](x: LogicStreamT[F, A], y: => LogicStreamT[F, A]): LogicStreamT[F, A] =
      x.mplus(y)

    override def msplit[A](c: LogicStreamT[F, A]): LogicStreamT[F, Option[(Try[A], LogicStreamT[F, A])]] = {
      c.msplit
    }

    override def mObserveOne[A](ma: LogicStreamT[F, A]): Observer[Option[A]] = {
      observerCpsMonad.flatMap(ma.fsplit) {
        case None =>
          observerCpsMonad.pure(None)
        case Some((head, tail)) =>
          head match
            case Success(a) =>
              observerCpsMonad.pure(Some(a))
            case Failure(e) =>
              observerCpsMonad.error(e)
      }
    }

    override def mFoldLeft[A, B](ma: LogicStreamT[F, A], zero: Observer[B])(op: (Observer[B], Observer[A]) => Observer[B]): Observer[B] = {
      observerCpsMonad.flatMap(ma.fsplit){
        case None =>
          zero
        case Some((head,tail)) =>
          head match
            case Success(a) =>
              val b1 = op(zero,observerCpsMonad.pure(a))
              mFoldLeft(tail,b1)(op)
            case Failure(e) =>
              observerCpsMonad.error(e)
      }
    }

    override def mFoldLeftN[A, B](ma: LogicStreamT[F, A], zero: Observer[B], n: Int)(op: (Observer[B], Observer[A]) => Observer[B]): Observer[B] = {
      if (n<=0) then
        zero
      else
        observerCpsMonad.flatMap(ma.fsplit){
          case None =>
            zero
          case Some((head,tail)) =>
            head match
              case Success(a) =>
                val b1 = op(zero,observerCpsMonad.pure(a))
                mFoldLeftN(tail,b1,n-1)(op)
              case Failure(e) =>
                observerCpsMonad.error(e)
        }
    }

  }

}
