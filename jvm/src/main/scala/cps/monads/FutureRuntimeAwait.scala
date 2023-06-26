package cps.monads

import cps.*
import cps.runtime.*
import scala.concurrent.*

object FutureRuntimeAwait extends LoomRuntimeAwait[Future] {

  override def submit(fu: Future[Unit])(ctx: CpsTryMonadContext[Future]): Unit = {
    // do nothing, because Future is already started.
  }

}

// TODO: enable after JVM-2.1
given futureRuntimeAwait(using cps.macros.flags.UseLoomAwait):CpsRuntimeAwait[Future] = FutureRuntimeAwait