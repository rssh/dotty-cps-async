package testUtil

import cps.*
import cps.monads.FreeMonad
import scala.util.*

extension [T](v: FreeMonad[T]) {

  def tryEval: Try[T] =
    try
      Success(v.eval)
    catch
      case ex: Throwable => Failure(ex)

  def eval: T =
    v match
      case FreeMonad.Pure(v) => v
      case FreeMonad.FlatMap(fa,f) =>
        val fv = fa.eval
        val next = f(fv)
        next.eval
      case FreeMonad.Error(v) =>
        throw v
      case FreeMonad.FlatMapTry(fa,f) =>
        val fv = fa.tryEval
        val next = f(fv)
        next.eval

}
