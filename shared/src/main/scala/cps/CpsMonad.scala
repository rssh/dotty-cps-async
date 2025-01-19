/*
 * dotty-cps-async: https://github.com/rssh/dotty-cps-async
 *
 * (C) Ruslan Shevchenko <ruslan@shevchenko.kiev.ua>, Kyiv, 2020, 2021, 2022, 2023
 */
package cps

import scala.quoted._
import scala.util._
import scala.util.control.NonFatal
import scala.concurrent.duration._
import java.util.concurrent.atomic.AtomicReference

/** Basic CpsMonad operations. Implementing this typeclass is enough to use async/await with supports of basic control-flow
  * constructions (if, loops, but no exceptions).
  */
trait CpsMonad[F[_]] {

  type WF[X] = F[X]

  type Context <: CpsMonadContext[F]

  /** Pure - wrap value `t` inside monad.
    *
    * Note, that pure use eager evaluation, which is different from Haskell.
    */
  def pure[T](t: T): F[T]

  /** map a function `f` over `fa`
    */
  def map[A, B](fa: F[A])(f: A => B): F[B]

  /** bind combinator, which compose `f` over `fa`
    */
  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

  /** run op in the context environment.
    */
  def apply[T](op: Context => F[T]): F[T]

  /** Create monadic expression according to the default operation of choosen monad types. (i.e. delaying for effect monads,
    * starting for eager monand, pure by default)
    */
  def wrap[T](op: => T): F[T] =
    map(pure(())) { _ => op }

  /** Wrap and flatten of monadic expression..
    * @param op
    * @tparam T
    * @return
    */
  def flatWrap[T](op: => F[T]): F[T] =
    flatMap(pure(())) { _ => op }

  def flatten[T](ffa: F[F[T]]): F[T] =
    flatMap(ffa)(x => x)

}

object CpsMonad {

  type Aux[F[_], C <: CpsMonadContext[F]] = CpsMonad[F] { type Context = C }

}

/** Throw support for monads, decoupled from monad itself.
  * @tparam F
  */
trait CpsThrowSupport[F[_]] {

  /** represent error `e` in monadic context.
    */
  def error[A](e: Throwable): F[A]

}

/** Monad, where we can throw error but can catch one only outside of a monad context.
  *
  * An example as monad which interpret in sync context, such as Option or collections.
  */
trait CpsThrowMonad[F[_]] extends CpsMonad[F] with CpsThrowSupport[F] {

  override type Context <: CpsThrowMonadContext[F]

}

trait CpsTrySupport[F[_]] extends CpsThrowSupport[F] {

  def flatMapTry[A, B](fa: F[A])(f: Try[A] => F[B]): F[B]

  def fromTry[A](ta: Try[A]): F[A]

  def withAsyncFinalizer[A](fa: => F[A])(f: => F[Unit]): F[A] = {
    flatMapTry {
      try fa
      catch
        case NonFatal(ex) =>
          error(ex)
    } { r =>
      try
        flatMapTry(f) { finR =>
          finR match
            case Success(_) =>
              fromTry(r)
            case Failure(finRex) =>
              r match
                case Failure(rEx) =>
                  finRex.addSuppressed(rEx)
                  error(finRex)
                case _ => error(finRex)
        }
      catch
        case NonFatal(ex) =>
          r match
            case Failure(rEx) => ex.addSuppressed(rEx)
            case _            =>
          error(ex)
    }
  }

  // TODO: think, mb switch to non-lazy eval and wrapo fa in try/catch during code generation
  def withAsyncErrorHandler[A](fa: => F[A])(f: Throwable => F[A]): F[A] = {
    flatMapTry {
      try fa
      catch
        case NonFatal(ex) =>
          error(ex)
    } { r =>
      r match
        case Failure(ex) => f(ex)
        case _           => fromTry(r)
    }
  }

}

/** If you monad supports this typeclass, than you can use try/catch/finally inside await.
  */
trait CpsTryMonad[F[_]] extends CpsThrowMonad[F] with CpsTrySupport[F] {

  override type Context <: CpsTryMonadContext[F]

  /** represent error `e` in monadic context.
    */
  def error[A](e: Throwable): F[A]

  /** flatMap over result of checked evaluation of `A`
    */
  def flatMapTry[A, B](fa: F[A])(f: Try[A] => F[B]): F[B]

  /** map over result of checked evaluation of `A`
    */
  def mapTry[A, B](fa: F[A])(f: Try[A] => B): F[B] =
    flatMapTry(fa)(x => pure(f(x)))

  /** synonym for flatMapTry needed for processing awaits inside mapTry.
    */
  def mapTryAsync[A, B](fa: F[A])(f: Try[A] => F[B]): F[B] =
    flatMapTry(fa)(f)

  /** restore fa, ie if fa sucessful - return fa, otherwise apply fx to received error.
    */
  def restore[A](fa: F[A])(fx: Throwable => F[A]): F[A] =
    flatMapTry[A, A](fa) { x =>
      x match
        case Success(a) => pure(a)
        case Failure(e) =>
          try {
            fx(e)
          } catch {
            case NonFatal(ex) => error(ex)
          }
    }

  /** ensure that `action` will run before getting value from `fa`
    */
  def withAction[A](fa: F[A])(action: => Unit): F[A] =
    flatMapTry(fa) { r =>
      try
        action
        fromTry(r)
      catch
        case NonFatal(ex) =>
          r match
            case Failure(mEx) => ex.addSuppressed(mEx)
            case _            =>
          error(ex)
    }

  /** async shift of `withAction`.
    *
    * This method is substituted instead withAction, when we use `await` inside `withAction` argument.
    */
  def withActionAsync[A](fa: F[A])(action: () => F[Unit]): F[A] =
    withAsyncAction(fa)(action())

  /** return result of `fa` after completition of `action`.
    */
  def withAsyncAction[A](fa: F[A])(action: => F[Unit]): F[A] =
    flatMapTry(fa) { ra =>
      try
        flatMapTry(action) { raa =>
          raa match
            case Success(_) => fromTry(ra)
            case Failure(raaex) =>
              ra match
                case Success(rav) =>
                case Failure(raex) =>
                  raaex.addSuppressed(raex)
              error(raaex)
        }
      catch
        case NonFatal(ex) =>
          ra match
            case Failure(raex) =>
              ex.addSuppressed(raex)
            case Success(_) =>
          error(ex)
    }

  /** try to evaluate synchonious operation and wrap successful or failed result into `F`.
    */
  def tryPure[A](a: => A): F[A] =
    try {
      pure(a)
    } catch {
      // TODO: handle control
      case NonFatal(ex) => error(ex)
    }

  /** async shift of tryPure.
    */
  def tryPureAsync[A](a: () => F[A]): F[A] =
    try {
      a()
    } catch {
      case NonFatal(ex) => error(ex)
    }

  /** try to evaluate async operation and wrap successful or failed result into `F`.
    */
  def tryImpure[A](a: => F[A]): F[A] =
    try {
      a
    } catch {
      case NonFatal(ex) => error(ex)
    }

  /** transform `r` into pure value or error.
    */
  def fromTry[A](r: Try[A]): F[A] =
    r match
      case Success(a)  => pure(a)
      case Failure(ex) => error(ex)

}

object CpsTryMonad {

  type Aux[F[_], C <: CpsMonadContext[F]] = CpsTryMonad[F] { type Context = C }

}

/** Monad, which is compatible with passing data via callbacks.
  *
  * Interoperability with Future: allows
  * ```
  *     async[F]{ .. await[Future](..) ... }
  * ```
  */
trait CpsAsyncMonad[F[_]] extends CpsTryMonad[F] {

  // override type Context <: CpsAsyncMonadContext[F]

  /** called by the source, which accept callback inside
    */
  def adoptCallbackStyle[A](source: (Try[A] => Unit) => Unit): F[A]

}

object CpsAsyncMonad {

  type Aux[F[_], C <: CpsMonadContext[F]] = CpsAsyncMonad[F] { type Context = C }

}

/** Marker trait, which mark effect monad, where actual evaluation of expression happens after building a monad, during effect
  * evaluation stage.
  *
  * evaluation of expression inside async block always delayed.
  */
trait CpsEffectMonad[F[_]] extends CpsMonad[F] {

  /** Delayed evaluation of unit. If you want to override this for you monad, you should overrid delayed to.
    */
  def delayedUnit: F[Unit] = pure(())

  /** For effect monads it is usually the same, as pure, but unlike pure, argument of evaluation is lazy.
    *
    * Note, that delay is close to original `return` in haskell with lazy evaluation semantics. So, for effect monads, representing
    * pure as eager function is a simplification of semantics, real binding to monads in math sence, should be with `delay` instead
    * `pure`
    */
  def delay[T](x: => T): F[T] = map(delayedUnit)(_ => x)

  /** shortcat for delayed evaluation of effect.
    */
  def flatDelay[T](x: => F[T]): F[T] = flatMap(delayedUnit)(_ => x)

  /** synonim for delay
    */
  override def wrap[T](op: => T): F[T] =
    delay(op)

  override def flatWrap[T](op: => F[T]): F[T] =
    flatDelay(op)

}

object CpsEffectMonad {

  type Aux[F[_], C <: CpsMonadContext[F]] = CpsEffectMonad[F] { type Context = C }

}

/** Effect with Try support
  */
trait CpsTryEffectMonad[F[_]] extends CpsEffectMonad[F] with CpsTryMonad[F]

/** Async Effect Monad
  */
trait CpsAsyncEffectMonad[F[_]] extends CpsAsyncMonad[F] with CpsTryEffectMonad[F]

/** Monad, where we can define an effect of starting operation in different execution flow.
  */
trait CpsConcurrentMonad[F[_]] extends CpsAsyncMonad[F] {

  /** Spawned[A] is a computation, which is executed in own flow. (i.e. Fiber, Future, etc ..)
    */
  type Spawned[A]

  /** spawn execution of operation in own execution flow.
    */
  def spawnEffect[A](op: => F[A]): F[Spawned[A]]

  /** join the `op` computation: i.e. result is `op` which will become available after the spawned execution will be done.
    */
  def join[A](op: Spawned[A]): F[A]

  /** Send cancel signal, which can be accepted or rejected by `op` flow.
    */
  def tryCancel[A](op: Spawned[A]): F[Unit]

  /** join two computations in such way, that they will execute concurrently.
    */
  def concurrently[A, B](fa: F[A], fb: F[B]): F[Either[(Try[A], Spawned[B]), (Spawned[A], Try[B])]] =
    flatMap(spawnEffect(fa)) { sa =>
      flatMap(spawnEffect(fb)) { sb =>
        val ref = new AtomicReference[Either[(Try[A], Spawned[B]), (Spawned[A], Try[B])] | Null](null)
        // apply{ ctx =>
        val endA = adoptCallbackStyle[Either[(Try[A], Spawned[B]), (Spawned[A], Try[B])]] { callback =>
          mapTry(join(sa)) { ra =>
            val v = Left(ra, sb)
            if ref.compareAndSet(null, v) then callback(Success(v))
          }
          mapTry(join(sb)) { rb =>
            val v = Right(sa, rb)
            if ref.compareAndSet(null, v) then callback(Success(v))
          }
        } // (using ctx)
        endA
        // }
      }
    }

}

object CpsConcurrentMonad {

  type Aux[F[_], C <: CpsMonadContext[F]] = CpsConcurrentMonad[F] { type Context = C }

}

/** Marker trait for concurrent effect monads.
  */
trait CpsConcurrentEffectMonad[F[_]] extends CpsConcurrentMonad[F] with CpsAsyncEffectMonad[F]

/** Monad, where we can spawn some event and be sure that one be evaluated, event if we drop result.
  *
  * Interoperability with Future: allows
  * ```
  *     async[Future]{
  *        ...
  *        await[F](..)
  *        ...
  *     }
  * ```
  */
trait CpsSchedulingMonad[F[_]] extends CpsConcurrentMonad[F] {

  /** schedule execution of op somewhere, immediatly. Note, that characteristics of scheduler can vary. TODO: rename to spawn until
    * we not stabilized
    */
  def spawnSync[A](op: => A): F[A] =
    spawn(pure(op))

  /** schedule execution of op somewhere, immediatly. Note, that characteristics of scheduler can vary.
    */
  def spawn[A](op: => F[A]): F[A]

  /** * In eager monad, spawned process can be represented by F[_]
    */
  type Spawned[A] = F[A]

  /** spawn the process of spawning. Used to unify concurrent operation in lazy and eager monads.
    */
  def spawnEffect[A](op: => F[A]): F[F[A]] =
    spawnSync(spawn(op))

  /** join spawned immediate monad means to receive those spawing monad.
    */
  def join[A](op: Spawned[A]): F[A] = op

  /** spawnSync
    */
  override def wrap[T](op: => T): F[T] =
    spawnSync(op)

  override def flatWrap[T](op: => F[T]): F[T] =
    spawn(op)

}

object CpsSchedulingMonad {

  type Aux[F[_], C <: CpsMonadContext[F]] = CpsSchedulingMonad[F] { type Context = C }

}
