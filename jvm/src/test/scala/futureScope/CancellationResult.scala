package futureScope

import scala.concurrent.Future
  

enum CancellationResult:
  case Cancelled, AlreadyFinished
  case Cancelling(finishing: Seq[Future[CancellationResult]])



  

  