package cps.monads.logic

import cps.*
import cps.stream.*

import java.util.concurrent.atomic.{AtomicBoolean, AtomicReference}
import scala.concurrent.*
import scala.util.*

/** Logic monad, where computation is done into known async concurrent monad F[_].
  */
trait CpsConcurrentLogicMonad[M[_], F[_]: CpsConcurrentMonad] extends CpsLogicMonad[M] {

  override type Observer[T] = F[T]

  override val observerCpsMonad: CpsConcurrentMonad[F] = summon[CpsConcurrentMonad[F]]

  /** convert M[A] to AsyncList[F,A]
    * @tparam A
    * @return
    *   AsyncList[F,A]
    */
  def toAsyncList[A](ma: M[A])(using ExecutionContext): AsyncList[F, A] =
    toStream[AsyncList[F, A], A](ma)

  /** convert M[A] to AsyncStream (i.e. FS2, AkkaStream, etc...)
    */
  def toStream[R, A](ma: M[A])(using R: CpsAsyncEmitAbsorber.Aux[R, F, ?, A]): R =
    val F = summon[CpsAsyncMonad[F]]
    R.eval { absorber => emitter =>
      reify[F] {
        var c = ma
        var done = false
        while (!done) {
          val next = reflect(fsplit(c))
          next match
            case None =>
              done = true
            case Some((a, rest)) =>
              c = rest
              a match
                case Success(a) =>
                  emitter.emit(a)
                case Failure(ex) =>
                  throw ex
                  ()
        }
      }
    }

  /** parallel or: a and b are evaluated concurrently in different fibers results are merged in one stream without delays.
    * @param a
    * @param b
    * @tparam A
    * @return
    */
  def parOr[A](a: M[A], b: M[A]): M[A] = {
    val firstFlag = new AtomicBoolean(false)
    def cont(
        r: Option[(Try[A], M[A])],
        other: observerCpsMonad.Spawned[Option[(Try[A], M[A])]],
        cb: Try[M[A]] => Unit
    ): F[Option[(Try[A], M[A])]] = {
      r match
        case None =>
          if (firstFlag.compareAndSet(false, true)) {
            observerCpsMonad.map(observerCpsMonad.join(other)) {
              case None =>
                cb(Success(empty[A]))
                None
              case Some((a, rest)) =>
                cb(Success(unsplit(a, rest)))
                r
            }
          } else observerCpsMonad.pure(r)
        case Some((a, rest)) =>
          if (firstFlag.compareAndSet(false, true)) {
            cb(
              Success(
                unsplit(
                  a,
                  parOr(
                    flattenObserver(
                      observerCpsMonad.flatMap(observerCpsMonad.join(other)) {
                        case None            => observerCpsMonad.pure(empty[A])
                        case Some((a, rest)) => observerCpsMonad.pure(unsplit(a, rest))
                      }
                    ),
                    rest
                  )
                )
              )
            )
          }
          observerCpsMonad.pure(r)
    }
    val fma = observerCpsMonad.adoptCallbackStyle[M[A]](cb =>
      async[F] {
        val spawnA = await(observerCpsMonad.spawnEffect(fsplit(a)))
        val spawnB = await(
          observerCpsMonad.spawnEffect(
            observerCpsMonad.flatMap(fsplit(b)) { r =>
              cont(r, spawnA, cb)
            }
          )
        )
        val ra = await(observerCpsMonad.join(spawnA))
        val unused = await(cont(ra, spawnB, cb))
      }
    )
    flattenObserver(fma)
  }

}

extension [M[_], F[_]: CpsConcurrentMonad, A](ma: M[A])(using m: CpsConcurrentLogicMonad[M, F])

  /** convert M[A] to AsyncList[F,A]
    * @tparam A
    * @return
    *   AsyncList[F,A]
    */
  def toAsyncList(using ExecutionContext): AsyncList[F, A] =
    m.toAsyncList(ma)

  /** convert M[A] to AsyncStream (i.e. FS2, AkkaStream, etc...)
    * @param R - async emitter for such type of stream
    * @tparam R - type of target stream
    */
  def toStream[R](using R: CpsAsyncEmitAbsorber.Aux[R, F, ?, A]): R =
    m.toStream[R, A](ma)

  /** parallel or.
    */
  def parOr(mb: M[A]): M[A] =
    m.parOr(ma, mb)

end extension

trait CpsConcurrentLogicMonadContext[M[_], F[_]] extends CpsLogicMonadContext[M] {

  override def monad: CpsConcurrentLogicMonad[M, F]

}

class CpsConcurrentLogicMonadInstanceContextBody[M[_], F[_]](m: CpsConcurrentLogicMonad[M, F])
    extends CpsConcurrentLogicMonadContext[M, F] {
  override def monad: CpsConcurrentLogicMonad[M, F] = m
}

trait CpsConcurrentLogicMonadInstanceContext[M[_], F[_]] extends CpsConcurrentLogicMonad[M, F] {

  override type Context = CpsConcurrentLogicMonadInstanceContextBody[M, F]

  override def apply[T](op: CpsConcurrentLogicMonadInstanceContextBody[M, F] => M[T]): M[T] = {
    op(new CpsConcurrentLogicMonadInstanceContextBody[M, F](this))
  }

}
