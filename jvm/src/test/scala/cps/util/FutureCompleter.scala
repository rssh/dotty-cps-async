package cps.util

import scala.concurrent._
import scala.concurrent.duration._


object FutureCompleter
{

  def apply[T](f: Future[T]): T =
    Await.result(f, 30.seconds)

}
