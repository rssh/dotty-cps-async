package cps.runtime.util

import cps.*

import scala.util.*
import scala.util.control.NonFatal

object BoundaryAsyncShift extends AsyncShift[boundary.type] {

  def apply[F[_], T](o: boundary.type, m: CpsTryMonad[F])(body: boundary.Label[T] => F[T]): F[T] = {
    val l = boundary.Label[T]()
    m.flatMapTry {
      try
        body(l)
      catch
        case NonFatal(ex) =>
          handleLabel(m, ex, l)
    } {
      case Success(value) =>
        m.pure(value)
      case Failure(ex) =>
        handleLabel(m, ex, l)
    }
  }


  def handleLabel[F[_],T](m:CpsTryMonad[F], ex:Throwable, l:boundary.Label[T]): F[T] = {
      val retval:F[T] = ex match
        case exb: boundary.Break[T]@unchecked =>
          if (exb.label eq l) then
            m.pure(exb.value)
          else
            m.error(ex)
        case _ =>
            m.error(ex)
      retval
  }

}
