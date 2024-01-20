package cps

import cps.stream.*
import scala.concurrent.*
import scala.util.*


/**
 * Logic monad, where computation is done into known async concurrent monad F[_].
 *
 * @tparam M
 * @tparam F
 */
trait CpsConcurrentLogicMonad[M[_],F[_]:CpsConcurrentMonad] extends CpsLogicMonad[M] {

  override type Observer[T] = F[T]

  override def observerCpsMonad: CpsConcurrentMonad[F] = summon[CpsConcurrentMonad[F]]

  /**
   * convert M[A] to AsyncList[F,A] 
   * @tparam A
   * @return  AsyncList[F,A]
   */
  def toAsyncList[A](ma:M[A])(using ExecutionContext): AsyncList[F,A] =
    toStream[AsyncList[F,A], A](ma)

  /**
   * convert M[A] to AsyncStream (i.e. FS2, AkkaStream, etc...)
   */
  def toStream[R,A](ma:M[A])(using R: CpsAsyncEmitAbsorber.Aux[R,F,?,A]): R =
    val F = summon[CpsAsyncMonad[F]]
    R.eval{ absorber => emitter =>
      reify[F]{
        var c = ma
        var done = false
        while(!done) {
          val next = reflect(fsplit(c))
          next match
            case None =>
              done = true
            case Some((a,rest)) =>
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
  

}

extension [M[_],F[_]:CpsConcurrentMonad,A](ma:M[A])(using m:CpsConcurrentLogicMonad[M,F])

  /**
   * convert M[A] to AsyncList[F,A] 
   * @tparam A
   * @return  AsyncList[F,A]
   */
  def toAsyncList(using ExecutionContext): AsyncList[F,A] =
    m.toAsyncList(ma)

  /**
   * convert M[A] to AsyncStream (i.e. FS2, AkkaStream, etc...)
   * @param R - async emitter for such type of stream
   * @tparam R - type of target stream
   */
  def toStream[R](using R: CpsAsyncEmitAbsorber.Aux[R,F,?,A]): R =
    m.toStream[R,A](ma)

end extension


trait CpsConcurrentLogicMonadContext[M[_],F[_]] extends CpsLogicMonadContext[M] {

  override def monad: CpsConcurrentLogicMonad[M,F]

}

class CpsConcurrentLogicMonadInstanceContextBody[M[_],F[_]](m:CpsConcurrentLogicMonad[M,F]) extends CpsConcurrentLogicMonadContext[M,F] {
  override def monad: CpsConcurrentLogicMonad[M,F] = m
}


trait CpsConcurrentLogicMonadInstanceContext[M[_],F[_]] extends CpsConcurrentLogicMonad[M,F]  {

  override type Context = CpsConcurrentLogicMonadInstanceContextBody[M,F]

  override def apply[T](op: CpsConcurrentLogicMonadInstanceContextBody[M,F] => M[T]): M[T] = {
    op(new CpsConcurrentLogicMonadInstanceContextBody[M,F](this))
  }

}
