package futureScope

import scala.concurrent.Future
  

enum CancellationResult:
  case Cancelled 
  case AlreadyFinished
  case Cancelling(finishing: Future[Unit])



  

  