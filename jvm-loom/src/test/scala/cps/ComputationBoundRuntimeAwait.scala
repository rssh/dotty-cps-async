package cps

import cps.runtime.*


object ComputationBoundRuntimeAwait extends LoomRuntimeAwait[ComputationBound] {

  override def submit(fa: ComputationBound[Unit])(m: CpsAsyncMonad[ComputationBound], ctx: CpsMonadContext[ComputationBound]): Unit = {
      val waiter = ComputationBound.spawn(fa)
  }


}
