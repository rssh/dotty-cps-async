package cps.runtime

import java.util.concurrent.CompletableFuture
import scala.concurrent.*
import scala.util.control.NonFatal
import cps.*


object CompletableFutureRuntimeAwait extends LoomRuntimeAwait[CompletableFuture] {

  override def runAsync[A, C <: _root_.cps.CpsTryMonadContext[CompletableFuture]](f: C => A)(m: CpsAsyncEffectMonad[CompletableFuture], ctx: C): CompletableFuture[A] = {
    val cf = new CompletableFuture[A]()
    Thread.ofVirtual().start(() => {
      try {
        val r = f(ctx)
        cf.complete(r)
      }catch{
        case NonFatal(ex) =>
          cf.completeExceptionally(ex)
      }
    })
    cf
  }

  override def await[A](fa: CompletableFuture[A])(ctx: CpsTryMonadContext[CompletableFuture]): A = {
    blocking(
      fa.get()  
    )
  }

}
