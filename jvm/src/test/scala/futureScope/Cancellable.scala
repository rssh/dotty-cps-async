package futureScope

import scala.concurrent.*
import scala.concurrent.duration.*


trait Cancellable {

  def cancel(ex: ScopeCancellationException): CancellationResult
  
}

trait IsCancellable[T] {
  def cancel(t:T, ex: ScopeCancellationException): CancellationResult
}

