package cps.compat

import scala.concurrent._
import cps._
import cps.monads.FutureContext
import cps.monads.{ given CpsAsyncMonad[Future] }

/**
 * Import this object instead cps.*  to receive SIP22-compabiliy interface.
 **/
object FutureAsync:

  inline def async[T](inline x:T)(using ec:ExecutionContext):Future[T]=
  {  
    FutureAsyncMonad(using ec).apply(ctx => transformFutureMonad(x,ctx) ) 
  }

  inline def transformFutureMonad[T](inline x:T, inline ctx: FutureContext): Future[T] = ${
    cps.macros.Async.transformMonad('x, '{ ctx.monad} , 'ctx )
  }

  inline def await[T](x:Future[T])(using ec: ExecutionContext):T = 
    val fm = FutureAsyncMonad(using ec)
    cps.await[Future, T, Future](x)(using FutureContext(fm), CpsMonadConversion.identityConversion[Future])


val sip22 = FutureAsync


