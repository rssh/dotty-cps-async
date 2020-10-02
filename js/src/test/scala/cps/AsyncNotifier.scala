package cps

import scala.concurrent.Future
import scala.scalajs._
import scala.scalajs.js._

class AsyncNotifier{

 def  doWait(): Future[Unit] = 
   currentPromise.toFuture 

 def  doNotifyAll(): Unit =
   val prevResolve = currentResolve
   createPromise()
   prevResolve(())
   
 def createPromise() = 
   currentPromise = new js.Promise((resolve, reject)=>{
      currentResolve = resolve;
      currentReject = reject;
   })

 var currentPromise: js.Promise[Unit] = _
 var currentResolve: js.Function1[|[Unit,Thenable[Unit]],?] = _
 var currentReject: js.Function1[Any,?] = _
 
}
