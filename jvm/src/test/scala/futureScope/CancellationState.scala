package futureScope

import scala.concurrent.Future

enum CancellationState:
  case Cancelled, AlreadyFinished, AlreadyCancelling
  case Blocked(future: CancellableFuture[Any], nextResult: Future[CancellationState])
  case PartiallyBlocked(blocks:Seq[Blocked]) 
