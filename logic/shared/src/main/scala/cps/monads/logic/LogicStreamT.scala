package cps.monads.logic

import cps.*
import cps.monads.{*, given}

import scala.collection.immutable.Queue
import scala.util.*
import scala.util.control.NonFatal

//
//  Version of LogiSeq with direct suspend (similar to haskell Control.Monad.Stream + LogicSeqT optimization)
//

type LogicStream[A] = LogicStreamT[CpsIdentity, A]

object LogicStream {

  def empty[A]: LogicStream[A] =
    LogicStreamT.Empty[CpsIdentity, A]()

  def pure[A](a: A): LogicStream[A] =
    LogicStreamT.Pure[CpsIdentity, A](a)

  def error[A](e: Throwable): LogicStream[A] =
    LogicStreamT.Error[CpsIdentity, A](e)

  def fromCollection[A](s: Iterable[A]): LogicStream[A] =
    s.foldLeft(empty[A])((s, a) => s.mplus(pure(a)))

  transparent inline def noChoices[A](using CpsLogicMonadContext[LogicStream]): A =
    reflect(empty[A])

}

sealed trait LogicStreamT[F[_]: CpsTryMonad, A] {

  def map[B](f: A => B): LogicStreamT[F, B]
  def flatMap[B](f: A => LogicStreamT[F, B]): LogicStreamT[F, B]

  def flatMapTry[B](f: Try[A] => LogicStreamT[F, B]): LogicStreamT[F, B]

  def mplus(other: => LogicStreamT[F, A]): LogicStreamT[F, A]

  def fsplit: F[Option[(Try[A], LogicStreamT[F, A])]]
  def msplit: LogicStreamT[F, Option[(Try[A], LogicStreamT[F, A])]]

}

object LogicStreamT {

  case class Empty[F[_]: CpsTryMonad, A]() extends LogicStreamT[F, A] {

    override def map[B](f: A => B): LogicStreamT[F, B] =
      Empty[F, B]()

    override def flatMap[B](f: A => LogicStreamT[F, B]): LogicStreamT[F, B] =
      Empty[F, B]()

    override def flatMapTry[B](f: Try[A] => LogicStreamT[F, B]): LogicStreamT[F, B] =
      Empty[F, B]()

    override def mplus(other: => LogicStreamT[F, A]): LogicStreamT[F, A] =
      other

    override def fsplit: F[Option[(Try[A], LogicStreamT[F, A])]] =
      summon[CpsTryMonad[F]].pure(None)

    override def msplit: LogicStreamT[F, Option[(Try[A], LogicStreamT[F, A])]] =
      mpure(None)

  }

  case class Pure[F[_]: CpsTryMonad, A](a: A) extends LogicStreamT[F, A] {

    override def map[B](f: A => B): LogicStreamT[F, B] =
      try Pure[F, B](f(a))
      catch case NonFatal(ex) => Error[F, B](ex)

    override def flatMap[B](f: A => LogicStreamT[F, B]): LogicStreamT[F, B] =
      try f(a)
      catch case NonFatal(ex) => Error[F, B](ex)

    override def flatMapTry[B](f: Try[A] => LogicStreamT[F, B]): LogicStreamT[F, B] =
      try f(Success(a))
      catch case NonFatal(ex) => Error[F, B](ex)

    override def mplus(other: => LogicStreamT[F, A]): LogicStreamT[F, A] =
      Cons(Success(a), () => other)

    override def fsplit: F[Option[(Try[A], LogicStreamT[F, A])]] =
      summon[CpsTryMonad[F]].pure(Some((Success(a), Empty[F, A]())))

    override def msplit: LogicStreamT[F, Option[(Try[A], LogicStreamT[F, A])]] =
      mpure(Some((Success(a), Empty[F, A]())))

  }

  case class Error[F[_]: CpsTryMonad, A](e: Throwable) extends LogicStreamT[F, A] {

    override def map[B](f: A => B): LogicStreamT[F, B] =
      Error[F, B](e)

    override def flatMap[B](f: A => LogicStreamT[F, B]): LogicStreamT[F, B] =
      Error[F, B](e)

    override def flatMapTry[B](f: Try[A] => LogicStreamT[F, B]): LogicStreamT[F, B] =
      try {
        f(Failure(e))
      } catch {
        case NonFatal(ex) => Error[F, B](ex)
      }

    override def mplus(other: => LogicStreamT[F, A]): LogicStreamT[F, A] =
      Cons(Failure(e), () => other)

    override def fsplit: F[Option[(Try[A], LogicStreamT[F, A])]] =
      summon[CpsTryMonad[F]].pure(Some((Failure(e), Empty[F, A]())))

    override def msplit: LogicStreamT[F, Option[(Try[A], LogicStreamT[F, A])]] =
      mpure(Some((Failure(e), Empty[F, A]())))

  }

  case class Cons[F[_]: CpsTryMonad, A](head: Try[A], tail: () => LogicStreamT[F, A]) extends LogicStreamT[F, A] {

    override def map[B](f: A => B): LogicStreamT[F, B] =
      head match
        case Success(a) =>
          try Cons[F, B](Success(f(a)), () => tail().map(f))
          catch case NonFatal(ex) => Error[F, B](ex)
        case Failure(ex) =>
          Error[F, B](ex)

    override def flatMap[B](f: A => LogicStreamT[F, B]): LogicStreamT[F, B] =
      head match
        case Success(a) =>
          try f(a).mplus(tail().flatMap(f))
          catch case NonFatal(ex) => Error[F, B](ex)
        case Failure(ex) =>
          Error[F, B](ex)

    override def flatMapTry[B](f: Try[A] => LogicStreamT[F, B]): LogicStreamT[F, B] =
      try f(head).mplus(tail().flatMapTry(f))
      catch case NonFatal(ex) => Error[F, B](ex)

    override def mplus(other: => LogicStreamT[F, A]): LogicStreamT[F, A] =
      MPlusSeq[F, A](Queue(this, other))

    override def fsplit: F[Option[(Try[A], LogicStreamT[F, A])]] =
      summon[CpsTryMonad[F]].pure(Some((head, tail())))

    override def msplit: LogicStreamT[F, Option[(Try[A], LogicStreamT[F, A])]] =
      mpure(Some((head, tail())))

  }

  case class MPlusSeq[F[_]: CpsTryMonad, A](queue: Queue[LogicStreamT[F, A]]) extends LogicStreamT[F, A] {

    override def map[B](f: A => B): LogicStreamT[F, B] =
      MPlusSeq[F, B](queue.map(_.map(f)))

    override def flatMap[B](f: A => LogicStreamT[F, B]): LogicStreamT[F, B] =
      MPlusSeq[F, B](queue.map(_.flatMap(f)))

    override def flatMapTry[B](f: Try[A] => LogicStreamT[F, B]): LogicStreamT[F, B] =
      MPlusSeq[F, B](queue.map(_.flatMapTry(f)))

    override def mplus(other: => LogicStreamT[F, A]): LogicStreamT[F, A] =
      MPlusSeq[F, A](queue.enqueue(Suspend(() => other)))

    override def fsplit: F[Option[(Try[A], LogicStreamT[F, A])]] =
      queue.dequeueOption match
        case None =>
          summon[CpsTryMonad[F]].pure(None)
        case Some((head, tail)) =>
          summon[CpsTryMonad[F]].flatMap(head.fsplit) {
            case None =>
              MPlusSeq[F, A](tail).fsplit
            case Some((headHead, tailHead)) =>
              if (tail.isEmpty) then summon[CpsTryMonad[F]].pure(Some((headHead, tailHead)))
              else summon[CpsTryMonad[F]].pure(Some((headHead, tailHead.mplus(MPlusSeq(tail)))))
          }

    override def msplit: LogicStreamT[F, Option[(Try[A], LogicStreamT[F, A])]] =
      queue.dequeueOption match
        case None =>
          mpure(None)
        case Some((head, tail)) =>
          head.msplit.flatMap {
            case None =>
              MPlusSeq[F, A](tail).msplit
            case Some((headHead, tailHead)) =>
              if (tail.isEmpty) then mpure(Some((headHead, tailHead)))
              else mpure(Some((headHead, tailHead.mplus(MPlusSeq(tail)))))
          }

  }

  case class WaitF[F[_]: CpsTryMonad, A](waited: F[LogicStreamT[F, A]]) extends LogicStreamT[F, A] {

    override def map[B](f: A => B): LogicStreamT[F, B] =
      WaitF[F, B](summon[CpsTryMonad[F]].map(waited)(_.map(f)))

    override def flatMap[B](f: A => LogicStreamT[F, B]): LogicStreamT[F, B] =
      WaitF[F, B](summon[CpsTryMonad[F]].map(waited)(_.flatMap(f)))

    override def flatMapTry[B](f: Try[A] => LogicStreamT[F, B]): LogicStreamT[F, B] =
      WaitF[F, B](summon[CpsTryMonad[F]].map(waited)(_.flatMapTry(f)))

    override def mplus(other: => LogicStreamT[F, A]): LogicStreamT[F, A] =
      MPlusSeq[F, A](Queue(this, Suspend(() => other)))

    override def fsplit: F[Option[(Try[A], LogicStreamT[F, A])]] =
      summon[CpsTryMonad[F]].flatMap(waited)(_.fsplit)

    override def msplit: LogicStreamT[F, Option[(Try[A], LogicStreamT[F, A])]] =
      WaitF[F, Option[(Try[A], LogicStreamT[F, A])]](
        summon[CpsTryMonad[F]].map(waited)(_.msplit)
      )

  }

  case class Suspend[F[_]: CpsTryMonad, A](suspended: () => LogicStreamT[F, A]) extends LogicStreamT[F, A] {

    override def map[B](f: A => B): LogicStreamT[F, B] =
      Suspend[F, B](() => suspended().map(f))

    override def flatMap[B](f: A => LogicStreamT[F, B]): LogicStreamT[F, B] =
      Suspend[F, B](() => suspended().flatMap(f))

    override def flatMapTry[B](f: Try[A] => LogicStreamT[F, B]): LogicStreamT[F, B] =
      Suspend[F, B](() => suspended().flatMapTry(f))

    override def mplus(other: => LogicStreamT[F, A]): LogicStreamT[F, A] =
      Suspend[F, A](() => suspended().mplus(other))

    override def fsplit: F[Option[(Try[A], LogicStreamT[F, A])]] =
      suspended().fsplit

    override def msplit: LogicStreamT[F, Option[(Try[A], LogicStreamT[F, A])]] =
      suspended().msplit
  }

  /** Empty stream
    */
  def empty[F[_]: CpsTryMonad, A](): LogicStreamT[F, A] =
    Empty[F, A]()

  def pure[F[_]: CpsTryMonad, A](a: A): LogicStreamT[F, A] =
    Pure[F, A](a)

  def error[F[_]: CpsTryMonad, A](e: Throwable): LogicStreamT[F, A] =
    Error[F, A](e)

  def mpure[F[_]: CpsTryMonad, A](a: A): LogicStreamT[F, A] =
    Pure[F, A](a)

  def fpure[F[_]: CpsTryMonad, A](fa: F[A]): LogicStreamT[F, A] =
    WaitF[F, A](summon[CpsTryMonad[F]].map(fa)(a => Pure[F, A](a)))

  given cpsLogicStreamConcurrentMonad[F[_]: CpsConcurrentMonad]: CpsConcurrentLogicMonad[[A] =>> LogicStreamT[F, A], F] =
    CpsLogicStreamConcurrenctMonad[F]()

  // TODO: check, move to low-leve in case of conflict.
  given cpsLogicStreamMonad: CpsLogicStreamSyncMonad.type = CpsLogicStreamSyncMonad

  def current[F[_]](using CpsLogicMonadContext[[A] =>> LogicStreamT[F, A]]): CpsLogicMonadContext[[A] =>> LogicStreamT[F, A]] =
    summon[CpsLogicMonadContext[[A] =>> LogicStreamT[F, A]]]

  given observeConversion[F[_]: CpsTryMonad]: CpsMonadConversion[F, [A] =>> LogicStreamT[F, A]] with
    def apply[T](ft: F[T]): LogicStreamT[F, T] =
      LogicStreamT.WaitF(summon[CpsTryMonad[F]].map(ft)(LogicStreamT.mpure(_)))

}

trait CpsLogicStreamMonadBase[F[_]: CpsTryMonad] extends CpsLogicMonad[[A] =>> LogicStreamT[F, A]] {

  // import LogicStreamT.*

  override type Observer[A] = F[A]

  override val observerCpsMonad: CpsTryMonad[F] = summon[CpsTryMonad[F]]

  override def pure[T](t: T): LogicStreamT[F, T] = LogicStreamT.mpure(t)(using observerCpsMonad)

  override def map[A, B](fa: LogicStreamT[F, A])(f: A => B): LogicStreamT[F, B] =
    fa.map(f)

  override def flatMap[A, B](fa: LogicStreamT[F, A])(f: A => LogicStreamT[F, B]): LogicStreamT[F, B] =
    fa.flatMap(f)

  override def flatMapTry[A, B](fa: LogicStreamT[F, A])(f: Try[A] => LogicStreamT[F, B]): LogicStreamT[F, B] =
    fa.flatMapTry(f)

  override def error[A](e: Throwable): LogicStreamT[F, A] =
    LogicStreamT.WaitF(observerCpsMonad.error(e))(using observerCpsMonad)

  override def mzero[A]: LogicStreamT[F, A] = LogicStreamT.Empty[F, A]()

  override def mplus[A](x: LogicStreamT[F, A], y: => LogicStreamT[F, A]): LogicStreamT[F, A] =
    x.mplus(y)

  override def seqOr[A](a: LogicStreamT[F, A], b: => LogicStreamT[F, A]): LogicStreamT[F, A] =
    a.mplus(b)

  override def msplit[A](c: LogicStreamT[F, A]): LogicStreamT[F, Option[(Try[A], LogicStreamT[F, A])]] = {
    c.msplit
  }

  override def fsplit[A](c: LogicStreamT[F, A]): F[Option[(Try[A], LogicStreamT[F, A])]] =
    c.fsplit

  override def flattenObserver[A](fma: F[LogicStreamT[F, A]]): LogicStreamT[F, A] =
    LogicStreamT.WaitF(fma)

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

  override def mFoldLeftWhileM[A, B](ma: LogicStreamT[F, A], zeroM: F[B], p: B => Boolean)(op: (F[B], F[A]) => F[B]): F[B] = {
    observerCpsMonad.flatMap(zeroM) { zero =>
      if (p(zero)) then
        observerCpsMonad.flatMap(ma.fsplit) {
          case None =>
            observerCpsMonad.pure(zero)
          case Some((head, tail)) =>
            head match
              case Success(a) =>
                val b1 = op(observerCpsMonad.pure(zero), observerCpsMonad.pure(a))
                mFoldLeftWhileM(tail, b1, p)(op)
              case Failure(e) =>
                observerCpsMonad.error(e)
        }
      else observerCpsMonad.pure(zero)
    }
  }

}

class CpsLogicStreamTryMonad[F[_]: CpsTryMonad]
    extends CpsLogicStreamMonadBase[F]
    with CpsLogicMonadInstanceContext[[A] =>> LogicStreamT[F, A]] {

  override type WF[T] = LogicStreamT[F, T]

  override type Observer[A] = F[A]

  override val observerCpsMonad: CpsTryMonad[F] = summon[CpsTryMonad[F]]

}

object CpsLogicStreamSyncMonad
    extends CpsLogicStreamMonadBase[CpsIdentity]
    with CpsSyncLogicMonad[[A] =>> LogicStream[A]]
    with CpsLogicMonadInstanceContext[[A] =>> LogicStream[A]] {

  override type WF[T] = LogicStream[T]

  override type Observer[A] = A

  override val observerCpsMonad: CpsTryMonad[CpsIdentity] = summon[CpsTryMonad[CpsIdentity]]

  override def toLazyList[T](m: LogicStream[T]): LazyList[T] = {
    m.fsplit match
      case None => LazyList.empty
      case Some((head, tail)) =>
        head match
          case Success(a) => LazyList.cons(a, toLazyList(tail))
          case Failure(e) => throw e
  }

}

class CpsLogicStreamConcurrenctMonad[F[_]: CpsConcurrentMonad]
    extends CpsLogicStreamMonadBase[F]
    with CpsConcurrentLogicMonad[[A] =>> LogicStreamT[F, A], F]
    with CpsConcurrentLogicMonadInstanceContext[[A] =>> LogicStreamT[F, A], F] {

  override type WF[T] = LogicStreamT[F, T]

  override type Observer[A] = F[A]

  override val observerCpsMonad: CpsConcurrentMonad[F] = summon[CpsConcurrentMonad[F]]

}
