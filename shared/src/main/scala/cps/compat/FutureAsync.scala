package cps.compat

import scala.concurrent._
import cps._
import cps.monads.{ given CpsAsyncMonad[Future] }

object FutureAsync:

  inline def async[T](inline x:T)(using ec:ExecutionContext):Future[T]=
  ${  
    cps.macros.Async.transformMonad('x, '{FutureAsyncMonad(using ec)} )
  }

  inline def await[T](x:Future[T])(using ec: ExecutionContext):T = 
                       cps.await[Future, T](x)(using FutureAsyncMonad(using ec))


val sip22 = FutureAsync


