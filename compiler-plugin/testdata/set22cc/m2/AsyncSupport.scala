package gears.async

import scala.concurrent.*
import scala.concurrent.duration.*
import cps.*

type AsyncContext = CpsDirect[JSAsync]

extension (actx: AsyncContext)
    
  def group: CompletionGroup =
    async.group
    
  inline def async: Async =
    actx.context.asInstanceOf[Async]  
    
@cps.plugin.annotation.CpsNotChange()
given asyncFromAsyncContext(using actx: AsyncContext): Async = actx.context.asInstanceOf[Async]

given cps.macros.flags.UseCompilerPlugin.type = cps.macros.flags.UseCompilerPlugin


/**
 * Analog of suspension in monadic is arrow.
 **/ 
trait Suspension[-T, +R]:

  def resume(arg:T)(using CpsDirect[JSAsync]): R


/** 
 *  A scheduler implementation, with the ability to execute a computation immediately or after a delay. 
 **/
trait Scheduler:
  def execute(body: Runnable)(using CpsDirect[JSAsync]): Unit
  def schedule(delay: FiniteDuration, body: Runnable): Cancellable


trait SuspendSupport:

  type Label[R]

  type Suspension[-T, +R] <: gears.async.Suspension[T,R]

  def boundary[R](body: Label[R] ?=> R)(using CpsDirect[JSAsync]): R



trait AsyncSupport extends SuspendSupport:

  type Scheduler <: gears.async.Scheduler

 


