package cps.runtime

import scala.concurrent.*
import scala.concurrent.duration.*
import scala.util.*
import scala.util.control.NonFatal
import cps.*
import java.util.concurrent.CompletableFuture

object FutureLoomRuntimeAwait extends LoomRuntimeAwait[Future] {
  
  override def await[A](fa: Future[A])(ctx: CpsTryMonadContext[Future]): A = {
        import scala.concurrent.ExecutionContext.Implicits.global
        val completableFuture = new CompletableFuture[A]()
        fa.onComplete {
          case Success(r) => completableFuture.complete(r)
          case Failure(ex) => completableFuture.completeExceptionally(ex)
        }
        // it's depend on implementation of thread pool,
        //    in some cases this is not needed (because thread pool create virtual thread for each task),
        //    but we can't know this. 
        blocking(
          completableFuture.get()
        )
    }

}


