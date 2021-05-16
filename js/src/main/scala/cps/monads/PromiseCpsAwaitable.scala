package cps


import scala.language.implicitConversions

import scalajs.*
import scala.concurrent.Future

given CpsAwaitable[js.Promise] with {}


given jsPromiseToFuture[T]: Conversion[js.Promise[T],Future[T]] = _.toFuture


