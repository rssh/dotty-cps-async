package cps.runtime.concurrent

import scala.concurrent.*
import cps.*

class FutureCMAsyncShift extends AsyncShift[Future.type] {

  // Note, that result is nof F[T], but Future[T]
  //  This will work when F is Future and produce error in other cases.
  //  TODO: some safe nechanism to make this situation explicit with clear error messaged.
  //    (annotations or other list of parameters)
  def apply[F[_],T](obj: Future.type, m:CpsMonad[F])(op: () => Future[T])(ec: ExecutionContext): Future[T] = {
    Future(op())(using ec).flatten
  }

}

object FutureCMAsyncShift extends FutureCMAsyncShift