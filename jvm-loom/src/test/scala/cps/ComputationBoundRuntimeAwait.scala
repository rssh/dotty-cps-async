package cps

import cps.runtime.*


object ComputationBoundRuntimeAwait extends LoomRuntimeAwait[ComputationBound] {

  override def submit(fa: ComputationBound[Unit])(m: CpsAsyncMonad[ComputationBound], ctx: CpsMonadContext[ComputationBound]): Unit = {
      //ComputationBound.spawn(fa)
      val deadline = System.currentTimeMillis + (60*60*24)*1000
      Loom.startVirtualThread{ () =>
         println(s"sComputationBoundRuntimeAwait:tarting process in vierual thread for $fa (before progoress)")
         fa.progressNoBlock(deadline, 0) match
            case r@Wait(ref,op) =>
              println(s"sComputationBoundRuntimeAwait: $r (after progoress)")
              ComputationBound.spawn(r)
            case r@Thunk(e) =>
              println(s"sComputationBoundRuntimeAwait: $r (after progoress)")
              ComputationBound.spawn(r)
            case Done(v) =>
              println(s"sComputationBoundRuntimeAwait: $v (after progoress)")
            case Error(ex) =>
              println(s"Unhandled exception in ComputationBound: $ex")
              ex.printStackTrace()
      }
  }


}
