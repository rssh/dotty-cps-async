package logic.logict

/**
 * 'Fixed' LogicT, represented as fast sequence of mplus/cons operations.
 *
 *  see:
 *  Atze van der Ploeg and Oleg Kiselyov. 2014. Reflection without remorse: revealing a hidden sequence to speed up monadic reflection.
 *  In Proceedings of the 2014 ACM SIGPLAN symposium on Haskell (Haskell '14).
 *  Association for Computing Machinery, New York, NY, USA, 133–144.
 *  https://doi.org/10.1145/2633357.2633360
 *
 *  Haskell implementations:
 *    - supplement to the paper code: https://github.com/atzeus/reflectionwithoutremorse
 *    - adopted to the standard package: https://hackage.haskell.org/package/logict-sequence
 *                (github: https://github.com/dagit/logict-sequence )
 *
 *  Adopted to Scala and CpsLogicMonad
 */

//translatr from Haskell supplementary code:
//newtype ML m a = ML ( CQueue (m (Maybe (a, ML m a))))
//  (note, that in haskell evaluation is lazu, scala analog
//
//  type ML[F[_],A] = Queue[F[Option[(Try[A],(ML[F,A])]]]
//
//  F should be an effect monad. [i.e. have delay]
//
//from logic-sequence:
//data ViewT m a = Empty | a :< SeqT m a
//newtype SeqT m a = SeqT (Queue (m (ViewT m a)))

import cps.*
import cps.monads.{*,given}

import scala.collection.immutable.Queue
import scala.util.*
import logic.*
import logic.logict.LogicSeqT.LQueue


import scala.annotation.unchecked.uncheckedVariance

sealed trait LogicSeqT[F[_]:CpsTryEffectMonad, A] {

  def map[B](f: A=>B): LogicSeqT[F,B]

  def flatMap[B](f: A=>LogicSeqT[F,B]): LogicSeqT[F,B]

  def flatMapTry[B](f: Try[A]=>LogicSeqT[F,B]): LogicSeqT[F,B]

  def fsplit: F[Option[(Try[A], LogicSeqT[F,A])]]

  def msplit: LogicSeqT[F,Option[(Try[A], LogicSeqT[F,A])]]

  def mplus(other: => LogicSeqT[F,A]): LogicSeqT[F,A]

  def queue: Queue[F[Option[(Try[A], LogicSeqT[F, A])]]]

  def appendQueue(y: Queue[F[Option[(Try[A], LogicSeqT[F, A])]]]): LogicSeqT[F,A]


}

type LogicSeqLM[A] = LogicSeqT[LazyValue, A]

type LogicSeqM[A] = LazyLogicSeqT[CpsIdentity, A]

object LogicSeqT {

  type Part[F[_],A] = Option[(Try[A], LogicSeqT[F,A])]

  case class LQueue[F[_]:CpsTryEffectMonad,A](override val queue: Queue[F[Option[(Try[A], LogicSeqT[F,A])]]] ) extends LogicSeqT[F,A] {

    override def map[B](f: A=>B): LogicSeqT[F,B] =
       LQueue(queue.map{ fa =>
          summon[CpsTryMonad[F]].map(fa){ opt =>
            opt.map{ case (ta,cont) =>
              (ta.map(f), cont.map(f))
            }
          }
        })

    override def flatMap[B](f: A=>LogicSeqT[F,B]): LogicSeqT[F,B] =
      LQueue(queue.map{ fa =>
        summon[CpsTryMonad[F]].flatMap(fa){ opt =>
          opt match
            case None => summon[CpsTryMonad[F]].pure(None)
            case Some((ta,cont)) =>
              ta match
                case Success(a) =>
                  val mAB = f(a)
                  mAB.mplus(cont.flatMap(f)).fsplit
                case Failure(ex) =>
                  summon[CpsTryMonad[F]].error(ex)
        }
      })

    override def flatMapTry[B](f: Try[A] => LogicSeqT[F, B]): LogicSeqT[F, B] = {
      LQueue(queue.map{ fa =>
        summon[CpsTryMonad[F]].flatMap(fa){ opt =>
          opt match
            case None => summon[CpsTryMonad[F]].pure(None)
            case Some((ta,cont)) =>
              val mAB = f(ta)
              mAB.mplus(cont.flatMapTry(f)).fsplit
        }
      })

    }

    override def fsplit: F[Option[(Try[A], LogicSeqT[F, A])]] =
      queue.dequeueOption match
        case None => summon[CpsTryMonad[F]].pure(None)
        case Some((fhead,tail)) =>
          summon[CpsTryMonad[F]].flatMap(fhead){ opt =>
            opt match
              case None => LQueue(tail).fsplit
              case Some((ta,cont)) =>
                summon[CpsTryMonad[F]].pure(Some((ta, cont.appendQueue(tail))))
          }

    override def msplit: LogicSeqT[F, Option[(Try[A], LogicSeqT[F, A])]] = {
        LogicSeqT.lift(fsplit)
    }

    override def mplus(other: => LogicSeqT[F,A]): LogicSeqT[F,A] =
      // here is a problem for an infinite sequencts.
      // we should have somethig like fully-suspended state
      //
      //LQueue(queue ++ other.queue)
      // instead: still lazy,
      val suspendendSecond = summon[CpsTryEffectMonad[F]].flatMap(
                                  summon[CpsTryEffectMonad[F]].delay(other))(_.fsplit)
      LQueue(queue.appended(suspendendSecond))




    override def appendQueue(y: Queue[F[Option[(Try[A], LogicSeqT[F, A])]]]): LogicSeqT[F,A] =
      LQueue(queue ++ y)

  }

  // to prevent creation of wrapper object
  case class Empty[F[_]:CpsTryEffectMonad,A]() extends LogicSeqT[F,A] {

    override def map[B](f: A=>B): LogicSeqT[F,B] =
      this.asInstanceOf[LogicSeqT[F,B]]

    override def flatMap[B](f: A=>LogicSeqT[F,B]): LogicSeqT[F,B] =
      this.asInstanceOf[LogicSeqT[F,B]]

    override def flatMapTry[B](f: Try[A] => LogicSeqT[F, B]): LogicSeqT[F, B] =
      this.asInstanceOf[LogicSeqT[F,B]]

    override def fsplit: F[Option[(Try[A], LogicSeqT[F, A])]] =
      summon[CpsTryMonad[F]].pure(None)

    override def msplit: LogicSeqT[F, Option[(Try[A], LogicSeqT[F, A])]] =
       this.asInstanceOf[LogicSeqT[F,Option[(Try[A], LogicSeqT[F,A])]]]

    override def mplus(other: => LogicSeqT[F,A]): LogicSeqT[F,A] =
      other

    override def queue: Queue[F[Option[(Try[A], LogicSeqT[F, A])]]] =
      Queue.empty

    override def appendQueue(y: Queue[F[Option[(Try[A], LogicSeqT[F, A])]]]): LogicSeqT[F,A] =
      LQueue(y)

  }

  def empty[F[_]:CpsTryEffectMonad,A]: LogicSeqT[F,A] =
    Empty()

  def mpure[F[_]:CpsTryEffectMonad,A](a:A): LogicSeqT[F,A] =
    LQueue(Queue(summon[CpsTryMonad[F]].pure(Some((Success(a), Empty.asInstanceOf[LogicSeqT[F,A]])))))

  def msingle[F[_]:CpsTryEffectMonad,A](a:A): LogicSeqT[F,Option[(Try[A],LogicSeqT[F,A])]] = {
    mpure(Some(Success(a), empty))
  }


  def lift[F[_],A](fa: F[A])(using CpsTryEffectMonad[F]): LogicSeqT[F,A] =
    LQueue(Queue(summon[CpsTryMonad[F]].map(fa){ a =>
      Some((Success(a), empty))
    }))

  class LogicSeqTCpsMonad[F[_]:CpsTryEffectMonad] extends CpsLogicMonad[[T]=>>LogicSeqT[F,T]]
                                                       with CpsLogicMonadInstanceContext[[T]=>>LogicSeqT[F,T]] {

    type Observer[A] = F[A]

    override def observerCpsMonad = summon[CpsTryEffectMonad[F]]

    override def pure[A](a:A):LogicSeqT[F,A] =
      LQueue(Queue(observerCpsMonad.pure(Some((Try(a),empty)))))

    override def map[A,B](fa:LogicSeqT[F,A])(f:A=>B):LogicSeqT[F,B] =
      fa.map(f)

    override def flatMap[A,B](fa:LogicSeqT[F,A])(f:A=>LogicSeqT[F,B]):LogicSeqT[F,B] =
      fa.flatMap(f)

    override def flatMapTry[A, B](fa: LogicSeqT[F, A])(f: Try[A] => LogicSeqT[F, B]): LogicSeqT[F, B] =
      fa.flatMapTry(f)

    override def error[A](e: Throwable): LogicSeqT[F, A] = {
      LQueue(Queue(observerCpsMonad.error(e)))
    }

    override def mzero[A]:LogicSeqT[F,A] = Empty()

    override def mplus[A](fa:LogicSeqT[F,A], fb: => LogicSeqT[F,A]):LogicSeqT[F,A] =
      fa.mplus(fb)

    override def msplit[A](fa:LogicSeqT[F,A]):LogicSeqT[F,Option[(Try[A], LogicSeqT[F,A])]] =
      fa.msplit

    override def mObserveOne[A](ma: LogicSeqT[F, A]): F[Option[A]] = {
      observerCpsMonad.flatMap(ma.fsplit)( opt =>
        opt match
          case None => observerCpsMonad.pure(None)
          case Some((ta,cont)) =>
            ta match
              case Success(a) =>
                observerCpsMonad.pure(Some(a))
              case Failure(ex) =>
                observerCpsMonad.error(ex)

      )
    }

    override def mFoldLeft[A, B](ma: LogicSeqT[F, A], zero: F[B])(op: (F[B], F[A]) => F[B]): F[B] = {
      observerCpsMonad.flatMap(ma.fsplit){ opt =>
        opt match
          case None => zero
          case Some((ta,cont)) =>
            ta match
              case Success(a) =>
                observerCpsMonad.flatMap(op(zero,observerCpsMonad.pure(a))){ b =>
                  mFoldLeft(cont, observerCpsMonad.pure(b))(op)
                }
              case Failure(ex) =>
                observerCpsMonad.error(ex)
      }
    }

    override def mFoldLeftN[A, B](ma: LogicSeqT[F, A], zero: F[B], n: Int)(op: (F[B], F[A]) => F[B]): F[B] = {
      if (n<=0) then
        zero
      else
        observerCpsMonad.flatMap(ma.fsplit){ opt =>
          opt match
            case None => zero
            case Some((ta,cont)) =>
              ta match
                case Success(a) =>
                  observerCpsMonad.flatMap(op(zero,observerCpsMonad.pure(a))){ b =>
                    mFoldLeftN(cont, observerCpsMonad.pure(b), n-1)(op)
                  }
                case Failure(ex) =>
                  observerCpsMonad.error(ex)
        }
    }

  }

  given logicSeqMonad[F[_]:CpsTryEffectMonad]: LogicSeqTCpsMonad[F] = LogicSeqTCpsMonad[F]()





}


// can't create opaque type with more than one type parameter ?
// TODO: fill bug report to dotty
//opaque type LazyLogicSeq[F[_]:CpsTryMonad,T] = LogicSeqT[[A]=>>LazyT[F,A],T]

class LazyLogicSeqT[F[_]:CpsTryMonad,T](val value: LogicSeqT[[A]=>>LazyT[F,A],T])

object LazyLogicSeqT {

  class LogicSeqLazyMonad[F[_] : CpsTryMonad](using lazyM: LogicSeqT.LogicSeqTCpsMonad[[A] =>> LazyT[F, A]])
    extends CpsLogicMonad[[X] =>> LazyLogicSeqT[F, X]] with CpsLogicMonadInstanceContext[[X] =>> LazyLogicSeqT[F, X]] {

    override type Observer[X] = F[X]

    override def observerCpsMonad = summon[CpsTryMonad[F]]

    override def pure[A](a: A): LazyLogicSeqT[F, A] =
      LazyLogicSeqT(lazyM.pure(a))

    override def map[A, B](fa: LazyLogicSeqT[F, A])(f: A => B): LazyLogicSeqT[F, B] =
      LazyLogicSeqT(fa.value.map(f))

    override def flatMap[A, B](fa: LazyLogicSeqT[F, A])(f: A => LazyLogicSeqT[F, B]): LazyLogicSeqT[F, B] =
      LazyLogicSeqT(fa.value.flatMap(a => f(a).value))

    override def flatMapTry[A, B](fa: LazyLogicSeqT[F, A])(f: Try[A] => LazyLogicSeqT[F, B]): LazyLogicSeqT[F, B] =
      LazyLogicSeqT(fa.value.flatMapTry(a => f(a).value))

    override def error[A](e: Throwable): LazyLogicSeqT[F, A] =
      LazyLogicSeqT(lazyM.error(e))

    override def mzero[A]: LazyLogicSeqT[F, A] =
      LazyLogicSeqT(lazyM.mzero)

    override def mplus[A](fa: LazyLogicSeqT[F, A], fb: => LazyLogicSeqT[F, A]): LazyLogicSeqT[F, A] =
      LazyLogicSeqT(lazyM.mplus(fa.value, fb.value ))

    override def msplit[A](fa: LazyLogicSeqT[F, A]): LazyLogicSeqT[F, Option[(Try[A], LazyLogicSeqT[F, A])]] =
      LazyLogicSeqT(lazyM.msplit(fa.value).map(_.map((ta, cont) => (ta, LazyLogicSeqT(cont)))))

    override def mObserveOne[A](ma: LazyLogicSeqT[F, A]): F[Option[A]] = {
      lazyM.mObserveOne(ma.value).get
    }

    override def mFoldLeft[A, B](ma: LazyLogicSeqT[F, A], zero: F[B])(op: (F[B], F[A]) => F[B]): F[B] = {
      lazyM.mFoldLeft(ma.value, LazyT[F,B](zero)){ (lb, la) =>
        LazyT(op(lb.get, la.get))
      }.get
    }

    override def mFoldLeftN[A, B](ma: LazyLogicSeqT[F, A], zero: F[B], n: Int)(op: (F[B], F[A]) => F[B]): F[B] = {
      lazyM.mFoldLeftN(ma.value, LazyT(zero), n){ (lb, la) =>
        LazyT(op(lb.get, la.get))
      }.get
    }

    

  }

  given logicSeqLazyMonad[F[_]](using tm: CpsTryMonad[F], nem: NotGiven[CpsTryEffectMonad[F]]): LogicSeqLazyMonad[F] = LogicSeqLazyMonad[F]()
  
}



