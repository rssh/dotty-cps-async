package futureScope

import scala.concurrent.*


trait Cancellable {

  def cancel(ex: ScopeCancellationException): CancellationResult
  
}

trait IsCancellable[T] {
  def cancel(t:T, ex: ScopeCancellationException): CancellationResult
}

trait CancellableFuture[A] extends Future[A] with Cancellable

class CancellablePromise[A](p:Promise[A]) extends Cancellable {

  override def cancel(ex: ScopeCancellationException): CancellationResult = {
    if p.tryFailure(ex) then
      CancellationResult.Cancelled
    else
      CancellationResult.AlreadyFinished
  }

}