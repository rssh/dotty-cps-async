package cps

import cps.runtime.*


object ComputationBoundRuntimeAwait extends LoomRuntimeAwait[ComputationBound] {

  override def submit(fa: ComputationBound[Unit])(m: CpsAsyncMonad[ComputationBound], ctx: CpsMonadContext[ComputationBound]): Unit = {
      val deadline = System.currentTimeMillis + (60*60*24)*1000
      fa.progressNoBlock(deadline, 0) 
  }


}
