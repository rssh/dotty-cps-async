package cps.runtime.util.control

import cps.*
import scala.util.*
import scala.util.control.*

class BreaksAsyncShift extends AsyncShift[Breaks.type] {

  def breakable[F[_]](o:Breaks.type,m:CpsTryMonad[F])(op: () => F[Unit]): F[Unit] = {
    m.flatMapTry{
      m.pure(Breaks.breakable( { op() }))
    }{ r =>
      r match
        case Success(v) => m.pure(v)
        case Failure(ex) =>
          try
            m.pure(Breaks.breakable( { throw ex }))
          catch
            case ex2: Throwable =>
              m.error(ex2)
    }
  }


}
