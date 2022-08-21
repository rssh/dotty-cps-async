package cps

import scala.concurrent.Future
import scala.concurrent.TimeoutException
import scala.concurrent.duration._
import scala.util._
import scala.scalajs._
import scala.scalajs.js._
import scala.scalajs.concurrent.JSExecutionContext.Implicits._


import cps.testconfig.given


class AsyncNotifier{

 var currentPromise: js.Promise[Unit] = _
 var currentResolve: js.Function1[|[Unit,Thenable[Unit]],?] = _
 var currentReject: js.Function1[Any,?] = _
 
 createPromise()


 def  timedWait(duration: Duration): Future[Unit] = 
    val nextPromise = new js.Promise[Unit]((resolve, reject) => {
          duration match {
             case finite: FiniteDuration =>
                 timers.setTimeout(finite)(
                            reject(new TimeoutException()))
             case _ => 
          }
          currentPromise.toFuture.onComplete{
            case Success(x) => resolve(x)
            case Failure(ex) => reject(ex)
          }
    })
    nextPromise.toFuture

 def  doNotifyAll(): Unit =
   val prevResolve = currentResolve
   createPromise()
   prevResolve(())
   
 private def createPromise() = 
   currentPromise = new js.Promise((resolve, reject)=>{
      currentResolve = resolve;
      currentReject = reject;
   })


}
