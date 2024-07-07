package gears.async

import scala.scalajs.js
import scala.scalajs.js.Promise

case class JSResolveReject[-R](resolve: (R) => Unit, reject: Throwable => Unit) {



}

case class JSResolveRejectWithPromise[R](resolveReject: JSResolveReject[R], promise: js.Promise[R])


case object JSResolveReject {



  def withPromise[R](): js.Promise[JSResolveRejectWithPromise[R]] = {
    var outResolve: (R) => Unit = null
    var outReject: (Throwable) => Unit = null
    val p = new js.Promise[R]((resolve, reject) => {
      outResolve = (x) => resolve(x)
      outReject = (ex) => reject(ex)
    })
    // now we want be sure, that assigments was done
    //  now we hope that second promise with evaluated after first, but it is not guaranteed
    // TODO:  in JS2024 exosts Promise.withResolvers which can be used here, but it is not yet available in scala.js.
    // TODO:  research and maybe wrap in interval
    val wrappedPromise = new Promise[JSResolveRejectWithPromise[R]]((resultResolve, resultReject) => {
      resultResolve(JSResolveRejectWithPromise(JSResolveReject(outResolve, outReject), p))
    })
    wrappedPromise
  }


}
