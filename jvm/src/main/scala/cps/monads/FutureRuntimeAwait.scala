package cps.monads

import cps.*
import cps.runtime.*
import scala.concurrent.*

object FutureRuntimeAwait extends LoomRuntimeAwait[Future] {

  override def submit(fu: Future[Unit])(m: CpsAsyncMonad[Future], ctx: CpsMonadContext[Future]): Unit = {
    // do nothing, because Future is already started.
  }

}

given CpsRuntimeAwait[Future] = FutureRuntimeAwait