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
        blocking {
          try
            completableFuture.get()
          catch
            case ec: ExecutionException =>
              throw ec.getCause()
        }
    }

}


