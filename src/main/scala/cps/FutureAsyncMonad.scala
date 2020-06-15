package cps

import scala.concurrent._
import scala.quoted._

object FutureAsyncMonad:
 // extends AsyncMonad[Future]

   type F[+T] = Future[T]

   //TODO: implement


