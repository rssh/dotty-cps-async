package cps.monads

import cps._

import scala.language.implicitConversions

import scalajs.*
import scala.concurrent.Future

given CpsMonadConversion[js.Promise, Future] with
  def apply[T](ft: js.Promise[T]): Future[T] = ft.toFuture
