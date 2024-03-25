package logic.logict

import cps.*
import cps.monads.{*, given}

import scala.util.*
import logic.*

import scala.annotation.tailrec

type LogicMSKFK[A] = LogicTSKFK[CpsIdentity,A]


given logicMonadM: LogicSKFK.LogicSKFKMonad[CpsIdentity] = LogicSKFK.logicMonadT[CpsIdentity]


/**
 * Dependency-less port of haskell's LogicT monad transformer.
 */
type LogicTSKFK[F[_],+A] = LogicSKFK.CallbackAcceptor[F,A]

object LogicSKFK {


  trait SuccessContinuation[F[_],-A,R] {

    def apply(ta: Try[A])(fk: =>F[R]): F[R]

  }


  trait CallbackAcceptor[F[_], +A] {
    /**
     *
     * @param sk - success continuation (like k wih continuation monad and s for succee)
     * @param fk - failure continuation.
     * @tparam A - type of value on the time of performing continuation
     * @return
     */
    def apply[R](sk: SuccessContinuation[F,A,R])(fK: =>F[R]): F[R]
  }



  class LogicSKFKMonad[F[_]:CpsTryMonad] extends CpsLogicMonad[[X]=>>CallbackAcceptor[F,X]]
                                          with CpsLogicMonadInstanceContext[[X]=>>CallbackAcceptor[F,X]] {

    override type Observer[A] = F[A]

    override def pure[A](a:A): CallbackAcceptor[F,A] = new CallbackAcceptor[F,A] {
      override def apply[R](sk: SuccessContinuation[F,A,R])(fk: =>F[R]):F[R] =
        sk(Success(a))(fk)
    }

    override def error[A](ex: Throwable): CallbackAcceptor[F,A] = new CallbackAcceptor[F,A] {
      override def apply[R](sk: SuccessContinuation[F,A,R])(fk: => F[R]): F[R] =
        sk(Failure(ex))(fk)
    }

    override def map[A, B](fa: CallbackAcceptor[F, A])(f: A => B): CallbackAcceptor[F, B] =
      new CallbackAcceptor[F,B] {
        override def apply[R](sk: SuccessContinuation[F,B,R])(fk: =>F[R]): F[R] =
          fa.apply[R]{
                new SuccessContinuation[F,A,R] {
                  override def apply(ta: Try[A])(fk: =>F[R]): F[R] =
                      ta match
                        case Success(a) =>
                          sk(Success(f(a)))(fk)
                        case Failure(ex) => sk(Failure(ex))(fk)
                }
          }(fk)
      }

    override def flatMap[A, B](fa: CallbackAcceptor[F, A])(f: A => CallbackAcceptor[F, B]): CallbackAcceptor[F, B] = {
      flatMapTry(fa) {
        case Success(a) => f(a)
        case Failure(ex) => error(ex)
      }
    }

    override def flatMapTry[A,B](fa: CallbackAcceptor[F,A])(f: Try[A] => CallbackAcceptor[F,B]): CallbackAcceptor[F,B] =
      new CallbackAcceptor[F,B] {
        override def apply[R](sk: SuccessContinuation[F,B,R])(fk: =>F[R]): F[R] =
          fa.apply[R](
            new SuccessContinuation[F,A,R] {
              override def apply(ta: Try[A])(fk: =>F[R]): F[R] =
                f(ta).apply[R](sk)(fk)
            }
          )(fk)
      }

    override def mzero[A]: CallbackAcceptor[F, A] =
      new CallbackAcceptor[F,A] {
        override def apply[R](sk: SuccessContinuation[F,A,R])(fk: =>F[R]):F[R] =
          fk
      }

    override def mplus[A](a: CallbackAcceptor[F, A], b: =>CallbackAcceptor[F, A]): CallbackAcceptor[F, A] =
      new CallbackAcceptor[F,A] {
        // scalability problem -- right argument deeper in stack.
        // in real world need to think about tramplining.
        override def apply[R](sk: SuccessContinuation[F,A,R])(fk: =>F[R]): F[R] =
          a.apply(sk)(b.apply(sk)(fk))
      }

    def lift[A](fa: =>F[A]): CallbackAcceptor[F,A] =
      new CallbackAcceptor[F,A] {
        override def apply[R](sk: SuccessContinuation[F,A,R])(fk: =>F[R]): F[R] =
           summon[CpsMonad[F]].flatMapTry(fa)(ta => sk(ta)(fk))
      }



    override def msplit[A](c: CallbackAcceptor[F, A]): CallbackAcceptor[F, Option[(Try[A], CallbackAcceptor[F, A])]] = {
        val fcr = c.apply[Option[(Try[A],CallbackAcceptor[F,A])]](
          new SuccessContinuation[F,A,Option[(Try[A],CallbackAcceptor[F,A])]] {
            override def apply(ta: Try[A])(fk: =>F[Option[(Try[A],CallbackAcceptor[F,A])]]): F[Option[(Try[A],CallbackAcceptor[F,A])]] =
              val next = flatMap(lift(fk)) {
                  case None => (mzero: CallbackAcceptor[F,A])
                  case Some((ta,sa)) => new CallbackAcceptor[F,A] {
                      override def apply[R](sk: SuccessContinuation[F,A,R])(fk: =>F[R]):F[R]  =
                          sk(ta)(sa.apply(sk)(fk))
                  }
              }
              summon[CpsMonad[F]].pure(Some((ta,next)))
          }
        )( summon[CpsMonad[F]].pure(None) )
        lift(fcr)
    }

    def prepend[A](a: A, c: CallbackAcceptor[F, A]): CallbackAcceptor[F, A] =
      new CallbackAcceptor[F,A] {
        override def apply[R](sk: SuccessContinuation[F,A,R])(fk: =>F[R]): F[R] =
          sk(Success(a))(c.apply(sk)(fk))
      }

    override def observerCpsMonad: CpsTryMonad[F] = summon[CpsTryMonad[F]]

    override def mObserveOne[A](m:CallbackAcceptor[F,A]): F[Option[A]] = {
      msplit(m).apply[Option[A]](
        new SuccessContinuation[F,Option[(Try[A],CallbackAcceptor[F,A])],Option[A]] {
          override def apply(ta: Try[Option[(Try[A],CallbackAcceptor[F,A])]])(fk: =>F[Option[A]]): F[Option[A]] =
            ta match
              case Success(v) => v match
                case None => fk
                case Some((ta,sa)) =>
                  ta match
                    case Success(a) =>
                      observerCpsMonad.pure(Some(a))
                    case Failure(ex) =>
                      observerCpsMonad.error(ex)
              case Failure(ex) =>
                observerCpsMonad.error(ex)
        })(summon[CpsMonad[F]].pure(None))
    }

    override def mFoldLeftN[A, B](ma: CallbackAcceptor[F, A], zero: F[B], n: Int)(op: (F[B], F[A]) => F[B]): F[B] = {
      if (n<=0) then
        zero
      else
        msplit(ma).apply[B](
           new SuccessContinuation[F,Option[(Try[A],CallbackAcceptor[F,A])],B] {
            override def apply(ta: Try[Option[(Try[A],CallbackAcceptor[F,A])]])(fk: =>F[B]): F[B] =
              ta match
                case Success(v) => v match
                  case None => fk
                  case Some((ta,sa)) =>
                    ta match
                      case Success(a) =>
                        mFoldLeftN(sa,op(zero,observerCpsMonad.pure(a)),n-1)(op)
                      case Failure(ex) =>
                        observerCpsMonad.error(ex)
                case Failure(ex) =>
                  observerCpsMonad.error(ex)
           })(zero)
    }

    override def mFoldLeft[A, B](ma: CallbackAcceptor[F, A], zero: F[B])(op: (F[B], F[A]) => F[B]): F[B] = {
      msplit(ma).apply[B](
        new SuccessContinuation[F,Option[(Try[A],CallbackAcceptor[F,A])],B] {
          override def apply(ta: Try[Option[(Try[A],CallbackAcceptor[F,A])]])(fk: =>F[B]): F[B] =
            ta match
              case Success(v) => v match
                case None => fk
                case Some((ta,sa)) =>
                  ta match
                    case Success(a) =>
                      mFoldLeft(sa,op(zero,observerCpsMonad.pure(a)))(op)
                    case Failure(ex) =>
                      observerCpsMonad.error(ex)
              case Failure(ex) =>
                observerCpsMonad.error(ex)
        })(zero)
    }

  }



  //given logicMonadSKF[F[_]:CpsTryMonad]: CpsLogicMonad[[X]=>>LogicCallbackAcceptor[F,X]]  =
  //  LogicSKFKMonad[F]()

  given logicMonadT[F[_] : CpsTryMonad]: LogicSKFKMonad[F] =
    LogicSKFKMonad[F]()


}
