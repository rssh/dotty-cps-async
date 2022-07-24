package futureScope

import scala.concurrent.*
import scala.concurrent.duration.*

import cps.*
import cps.monads.*
import cps.testconfig.given

trait CancellableFuture[+A] extends Future[A] with Cancellable

object CancellableFuture {

     // TODO: implement CpsMonad
     given CpsAwaitable[CancellableFuture] with {}

     given CpsMonadConversion[CancellableFuture,Future] with
      def apply[T](ft:CancellableFuture[T]): Future[T] = ft
 
}


class DelegatedCancellableFuture[+A](val origin: Future[A], cancelFun: ScopeCancellationException => CancellationResult) extends CancellableFuture[A] {

  export origin.onComplete
  export origin.isCompleted
  export origin.value
  export origin.transform
  export origin.transformWith
  export origin.result

  override def ready(atMost: Duration)(implicit permit: CanAwait): this.type = {
      origin.ready(atMost)
      this
  }

  override def cancel(ex: ScopeCancellationException): CancellationResult =
    cancelFun(ex)

}



class PromiseCancellableFuture[A](val p: Promise[A]) extends DelegatedCancellableFuture(p.future, ex => PromiseCancellableFuture.cancel(p,ex) ) 


object PromiseCancellableFuture {

  def cancel[A](p:Promise[A],ex: ScopeCancellationException): CancellationResult =
    if (p.tryFailure(ex)) then
      CancellationResult.Cancelled
    else
      CancellationResult.AlreadyFinished

}


