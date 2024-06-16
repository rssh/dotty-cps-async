package gears.async

import scala.concurrent.duration.FiniteDuration
import scala.util.*
import scala.util.control.NonFatal
import scala.scalajs.*
import scala.scalajs.js.timers.*
import cps.*

import scala.scalajs.js.{JavaScriptException, Promise, Thenable}
import scala.util.boundary.Break


object JSScheduler extends Scheduler:

  val resolvedUnitPromise = js.Promise.resolve(())

  def execute(body: Runnable)(using AsyncContext): Unit = {
     val p = js.Promise[Unit]((resolve, reject) => {
       resolvedUnitPromise.`then`( (_: Unit) => {
         try
            body.run()
         catch
           case NonFatal(e) =>
             e match
               case e: JavaScriptException =>
                 reject(e.exception)
               case other =>
                 reject(e)
       })
     })
     await(p)
  }

  def schedule(delay: FiniteDuration, body: Runnable): Cancellable = {
    val handle = setTimeout(delay.toMillis.toDouble)(body.run())
    //() => clearTimeout(handle)
    ???
  }


end JSScheduler


object JSAsyncSupport extends AsyncSupport:

  override type Scheduler = JSScheduler.type

  val scheduler: Scheduler = JSScheduler

  case class JSLabel[R](
            var wasSuspended: Boolean,
            bodyRrp: JSResolveRejectWithPromise[R],
            suspendRrp: JSResolveRejectWithPromise[R])



  type Label[R] = JSLabel[R]

  case class JSSuspension[-T, +R](trr: JSResolveReject[T], bp: js.Promise[R]) extends gears.async.Suspension[T, R] {

    def resume(t: T)(using AsyncContext): R = {
      trr.resolve(t)
      await(bp)
    }

  }

  type Suspension[-T,+R] = JSSuspension[T,R]

  def boundary[R](body: Label[R] ?=> R)(using AsyncContext): R = {
    boundary_async[R](label => JSAsync.eval(body(using label)))
  }

  def boundary_async[R](body: Label[R] => JSAsync[R])(using AsyncContext): R = {
    val bodyRrp = await(JSResolveReject.withPromise[R]())
    val suspendRrp = await(JSResolveReject.withPromise[R]())
    val label: Label[R] = JSLabel(false, bodyRrp, suspendRrp)
    try
      body(label).onComplete {
        case Success(x) =>
          label.bodyRrp.resolveReject.resolve(x)
          if (!label.wasSuspended) then
            label.suspendRrp.resolveReject.resolve(x)
        case Failure(ex) =>
          label.bodyRrp.resolveReject.reject(ex)
          if (!label.wasSuspended) then
            label.suspendRrp.resolveReject.reject(ex)
      }
    catch
      case NonFatal(e) =>
        label.bodyRrp.resolveReject.reject(e)
        if (!label.wasSuspended) {
          label.suspendRrp.resolveReject.reject(e)
        }

    await(label.suspendRrp.promise)
  }

  def suspend[T, R](body: Suspension[T,R] => R)(using Label[R], CpsDirect[JSAsync]): T = {
     suspend_async[T,R](s => JSAsync.eval(body(s)))
  }

  def suspend_async[T, R](body: Suspension[T,R] => JSAsync[R])(using Label[R], CpsDirect[JSAsync]): T = {
     val tRrp = await(JSResolveReject.withPromise[T]())
     summon[Label[R]].wasSuspended = true

     val suspension = JSSuspension(tRrp.resolveReject,summon[Label[R]].bodyRrp.promise)
     try
       body(suspension).onComplete{
          case Success(r) =>
            summon[Label[R]].suspendRrp.resolveReject.resolve(r)
          case Failure(ex) =>
            summon[Label[R]].suspendRrp.resolveReject.reject(ex)
       }
     catch
        case NonFatal(e) =>
          summon[Label[R]].suspendRrp.resolveReject.reject(e)

     await(tRrp.promise)
  }


end JSAsyncSupport

