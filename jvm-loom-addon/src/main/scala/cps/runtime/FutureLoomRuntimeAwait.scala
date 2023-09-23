package cps.runtime

import scala.concurrent.*
import scala.concurrent.duration.*
import scala.util.*
import scala.util.control.NonFatal
import cps.*
import java.util.concurrent.CompletableFuture

object FutureLoomRuntimeAwait extends LoomRuntimeAwait[Future] {

  override def runAsync[A, C <: CpsTryMonadContext[Future]](f: C => A)(m: CpsAsyncEffectMonad[Future], ctx: C): Future[A] = {
     val p = Promise[A]()
     Thread.ofVirtual().start(() => {
       try
          val r = f(ctx)
          p.success(r)
       catch
         case NonFatal(ex) =>
            p.failure(ex)
     })
     p.future
  }

  override def await[A](fa: Future[A])(ctx: CpsTryMonadContext[Future]): A = {
        import scala.concurrent.ExecutionContext.Implicits.global
        val completableFuture = new CompletableFuture[A]()
        fa.onComplete {
          case Success(r) => completableFuture.complete(r)
          case Failure(ex) => completableFuture.completeExceptionally(ex)
        }
        // it's depend on implementation of thread pool,
        //    in some cases this is not needed (because thread pool create virtual thread for each task),
        //    but we can't  know it. 
        blocking(
          completableFuture.get()
        )

    }

}


