package cmlib

import scala.annotation.experimental
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

import cps.*
import cps.monads.{*, given}

@experimental
object NetworkAPI {

  def fetch(url:String)(using CpsDirect[Future]): String = {
    s"fetched from $url"
  }

}