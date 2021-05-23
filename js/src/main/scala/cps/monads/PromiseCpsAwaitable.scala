package cps.monads

import cps._

import scala.language.implicitConversions

import scalajs.*
import scala.concurrent.Future

given CpsAwaitable[js.Promise] with {}

given CpsMonadMemoization[js.Promise] = CpsMonadDefaultMemoization[js.Promise]()

given CpsMonadConversion[js.Promise, Future] with
   def apply[T](ft: js.Promise[T]): Future[T] = ft.toFuture

