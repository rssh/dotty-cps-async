package cps.pe

import scala.concurrent.*
import scala.concurrent.duration.*

class PureEffectDispatcher(ec: ExecutionContext) {

  def dispatch[T](f: => PureEffect[T]): Future[T] = {
    given ExecutionContext = ec
    f.unsafeRunFuture()
  }

}

object PureEffectDispatcher {

  def apply(ec: ExecutionContext): PureEffect[PureEffectDispatcher]
     = PureEffect.delay( new PureEffectDispatcher(ec) )

}