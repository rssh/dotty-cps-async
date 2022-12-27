package cps.runtime.util.control

import scala.util.control.*


/**
 * This wrapper for control exception, which is handled by NonFatal(ex)
 * 
 * The main reason for it;'s existence, that we can't rewrite all
 * monadic operation to handle NonFatalOnly throwables instead NonFatal. 
 * (The right way will be to change monadic operations in standard library, but
 *  this is a long way.
 * )
 *
 * So, for supports of returning and break clauses, we change 
 * NonLocalReturns.throwReturn(ce) to NonLocalReturnsShift.throwAsyncWrapperReturn(ce)
 **/
class ControlThrowableAsyncWrapper(val ce:ControlThrowable) extends Throwable(
  ce.getMessage, ce, /*enableSuppression=*/false, /*writableStackTrace=*/false
) 


object NonFatalAndNotControlThrowableAsyncWrapper {

  def apply(t:Throwable): Boolean = t match {
    case _: VirtualMachineError | _: ThreadDeath | _: InterruptedException | _: LinkageError | _: ControlThrowable | _: ControlThrowableAsyncWrapper => false
    case _ => true
  }

  def unapply(e:Throwable): Option[Throwable] = if (apply(e)) Some(e) else None

}