package cps.runtime

import java.util.concurrent.CompletableFuture
import scala.concurrent.*
import scala.util.control.NonFatal
import cps.*

object CompletableFutureRuntimeAwait extends LoomRuntimeAwait[CompletableFuture] {

  override def await[A](fa: CompletableFuture[A])(ctx: CpsTryMonadContext[CompletableFuture]): A = {
    blocking {
      try fa.get()
      catch
        case ex: ExecutionException =>
          throw ex.getCause()
    }
  }

}
