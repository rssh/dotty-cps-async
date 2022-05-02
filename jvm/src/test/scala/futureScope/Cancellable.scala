package futureScope

import scala.concurrent.*

trait Cancellable {

  def cancel(): CancellationState

}


trait CancellableFuture[A] extends Future[A] with Cancellable
