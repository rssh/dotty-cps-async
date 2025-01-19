package cps.runtime.util

import scala.util._
import cps._

object UsingAsyncShift extends AsyncShift[Using.type]:

  def apply[F[_], R, A](o: Using.type, m: CpsTryMonad[F])(
      resource: () => F[R]
  )(f: (R) => F[A])(implicit arg0: Using.Releasable[R]): F[Try[A]] = {
    m.flatMap(resource())(r =>
      m.restore(
        m.map(f(r))(x => { arg0.release(r); Success(x) })
      )(e => m.pure(Failure(e)))
    )
  }

  def resource[F[_], R, A](o: Using.type, m: CpsTryMonad[F])(r: R)(f: (R) => F[A])(implicit arg0: Using.Releasable[R]): F[A] =
    m.withAction(f(r))(arg0.release(r))

  def resources[F[_], R1, R2, A](o: Using.type, m: CpsTryMonad[F])(r1: R1, r2: () => F[R2])(
      f: (R1, R2) => F[A]
  )(implicit r1Releasable: Using.Releasable[R1], r2Releasable: Using.Releasable[R2]): F[A] =
    m.withAction(
      m.flatMap(r2()) { r2x =>
        m.withAction(f(r1, r2x))(r2Releasable.release(r2x))
      }
    )(r1Releasable.release(r1))

  def resources[F[_], R1, R2, R3, A](o: Using.type, m: CpsTryMonad[F])(r1: R1, r2: () => F[R2], r3: () => F[R3])(
      f: (R1, R2, R3) => F[A]
  )(implicit r1Releasable: Using.Releasable[R1], r2Releasable: Using.Releasable[R2], r3Releasable: Using.Releasable[R3]): F[A] =
    m.withAction(
      m.flatMap(r2()) { r2x =>
        m.withAction(
          m.flatMap(r3())(r3x => m.withAction(f(r1, r2x, r3x))(r3Releasable.release(r3x)))
        )(r2Releasable.release(r2x))
      }
    )(r1Releasable.release(r1))

  def resources[F[_], R1, R2, R3, R4, A](
      o: Using.type,
      m: CpsTryMonad[F]
  )(r1: R1, r2: () => F[R2], r3: () => F[R3], r4: () => F[R4])(
      f: (R1, R2, R3, R4) => F[A]
  )(implicit r1Rl: Using.Releasable[R1], r2Rl: Using.Releasable[R2], r3Rl: Using.Releasable[R3], r4Rl: Using.Releasable[R4]): F[A] =
    UsingAsyncShift.resources[F, R1, R2, R3, A](o, m)(r1, r2, r3)((r1x, r2x, r3x) =>
      m.flatMap(r4())(r4x => m.withAction(f(r1x, r2x, r3x, r4x))(r4Rl.release(r4x)))
    )
