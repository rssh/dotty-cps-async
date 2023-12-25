package logic.logict

import cps.*
import cps.monads.{*, given}

import scala.util.*
import logic.*

import scala.annotation.tailrec

type LogicM[A] = LogicT[CpsIdentity,A]


given logicMonadM: LogicT.LogicSKFKMonad[CpsIdentity] = LogicT.logicMonadT[CpsIdentity]


/**
 * Dependency-less port of haskell's LogicT monad transformer.
 */
type LogicT[F[_],+A] = LogicT.LogicCallbackAcceptor[F,A]

object LogicT {


  type SuccessContinuation[F[_],A,R] = Try[A]=>F[R]=>F[R]


  trait LogicCallbackAcceptor[F[_],+A] {
    /**
     *
     * @param sk - success continuation (like k wih continuation monad and s for succee)
     * @param fk - failure continuation.
     * @tparam A - type of value on the time of performing continuation
     * @return
     */
    def apply[R](sk: SuccessContinuation[F,A,R]): F[R] => F[R]
  }



  class LogicSKFKMonad[F[_]:CpsTryMonad] extends CpsLogicMonad[[X]=>>LogicCallbackAcceptor[F,X]]
                                          with CpsLogicMonadInstanceContext[[X]=>>LogicCallbackAcceptor[F,X]] {

    override type Observer[A] = F[A]

    override def pure[A](a:A): LogicCallbackAcceptor[F,A] = new LogicCallbackAcceptor[F,A] {
      override def apply[R](sk: SuccessContinuation[F,A,R]): F[R] => F[R] =
        sk(Success(a))
    }

    override def error[A](ex: Throwable): LogicCallbackAcceptor[F,A] = new LogicCallbackAcceptor[F,A] {
      override def apply[R](sk: SuccessContinuation[F,A,R]): F[R] => F[R] =
        sk(Failure(ex))
    }
    
    override def map[A, B](fa: LogicCallbackAcceptor[F, A])(f: A => B): LogicCallbackAcceptor[F, B] =
      new LogicCallbackAcceptor[F,B] {
        override def apply[R](sk: SuccessContinuation[F,B,R]): F[R] => F[R] =
          fa.apply[R]{
            case Success(a) => sk(Success(f(a)))
            case Failure(ex) => sk(Failure(ex))
          }
      }

    override def flatMap[A, B](fa: LogicCallbackAcceptor[F, A])(f: A => LogicCallbackAcceptor[F, B]): LogicCallbackAcceptor[F, B] = {
      flatMapTry(fa) {
        case Success(a) => f(a)
        case Failure(ex) => error(ex)
      }
    }

    override def flatMapTry[A,B](fa: LogicCallbackAcceptor[F,A])(f: Try[A] => LogicCallbackAcceptor[F,B]): LogicCallbackAcceptor[F,B] =
      new LogicCallbackAcceptor[F,B] {
        override def apply[R](sk: SuccessContinuation[F,B,R]): F[R] => F[R] =
          fa.apply[R](
            {
              case Success(a) => f(Success(a)).apply(sk)
              case Failure(ex) => sk(Failure(ex))
            }
          )
      }

    override def mzero: LogicCallbackAcceptor[F, Nothing] =
      new LogicCallbackAcceptor[F,Nothing] {
        override def apply[R](sk: SuccessContinuation[F,Nothing,R]): (F[R] => F[R]) =
          identity[F[R]]
      }

    override def mplus[A](a: LogicCallbackAcceptor[F, A], b: LogicCallbackAcceptor[F, A]): LogicCallbackAcceptor[F, A] =
      new LogicCallbackAcceptor[F,A] {
        override def apply[R](sk: SuccessContinuation[F,A,R]): (F[R] => F[R]) =
           fk => a.apply(sk)(b.apply(sk)(fk))
      }

    def lift[A](fa:F[A]): LogicCallbackAcceptor[F,A] =
      new LogicCallbackAcceptor[F,A] {
        override def apply[R](sk: SuccessContinuation[F,A,R]): F[R] => F[R] =
          fk => summon[CpsMonad[F]].flatMapTry(fa)(ta => sk(ta)(fk))
      }



    override def msplit[A](c: LogicCallbackAcceptor[F, A]): LogicCallbackAcceptor[F, Option[(Try[A], LogicCallbackAcceptor[F, A])]] = {
        val fcr = c.apply[Option[(Try[A],LogicCallbackAcceptor[F,A])]](
          { c => cfk =>
             val next = flatMap(lift(cfk)) {
                  case None => (mzero: LogicCallbackAcceptor[F,A])
                  case Some((ta,sa)) => new LogicCallbackAcceptor[F,A] {
                      override def apply[R](sk: SuccessContinuation[F,A,R]): (F[R] => F[R]) =
                          fk => sk(ta)(sa.apply(sk)(fk))
                  }
             }
             summon[CpsMonad[F]].pure(Some((c,next)))
          }
        )( summon[CpsMonad[F]].pure(None) )
        lift(fcr)
    }

    def prepend[A](a: A, c: LogicCallbackAcceptor[F, A]): LogicCallbackAcceptor[F, A] =
      new LogicCallbackAcceptor[F,A] {
        override def apply[R](sk: SuccessContinuation[F,A,R]): (F[R] => F[R]) =
          fk => sk(Success(a))(c.apply(sk)(fk))
      }

    override def observerCpsMonad: CpsTryMonad[F] = summon[CpsTryMonad[F]]

    override def mObserveOne[A](m:LogicCallbackAcceptor[F,A]): F[Option[A]] = {
      msplit(m).apply[Option[A]] {
        case Success(v) => v match
          case None => fk => fk
          case Some((ta,sa)) => fk =>
            ta match
              case Success(a) => observerCpsMonad.pure(Some(a))
              case Failure(ex) => observerCpsMonad.error(ex)
        case Failure(ex) => fk => summon[CpsTryMonad[F]].error(ex)
      }(summon[CpsMonad[F]].pure(None))
    }

    override def mFoldLeftN[A, B](ma: LogicCallbackAcceptor[F, A], zero: F[B], n: Int)(op: (F[B], F[A]) => F[B]): F[B] = {
      if (n<=0) then
        zero
      else
        msplit(ma).apply[B] {
          case Success(v) => v match
            case None => fk => fk
            case Some((ta,sa)) =>
              ta match
                case Success(a) =>
                   fk => mFoldLeftN(sa, op(fk,observerCpsMonad.pure(a)), n-1)(op)
                case Failure(ex) =>
                   fk => observerCpsMonad.error(ex)
        }(zero)
    }

    override def mFoldLeft[A, B](ma: LogicCallbackAcceptor[F, A], zero: F[B], op: (F[B], F[A]) => F[B]): F[B] = {
      msplit(ma).apply[B] {
        case Success(v) => v match
          case None => fk => fk
          case Some((ta,sa)) => fk =>
            ta match
              case Success(a) =>
                mFoldLeft(sa, op(fk,observerCpsMonad.pure(a)), op)
              case Failure(ex) =>
                observerCpsMonad.error(ex)
        case Failure(ex) => fk => observerCpsMonad.error(ex)
      }(zero)
    }

  }



  //given logicMonadSKF[F[_]:CpsTryMonad]: CpsLogicMonad[[X]=>>LogicCallbackAcceptor[F,X]]  =
  //  LogicSKFKMonad[F]()

  given logicMonadT[F[_] : CpsTryMonad]: LogicSKFKMonad[F] =
    LogicSKFKMonad[F]()


}
