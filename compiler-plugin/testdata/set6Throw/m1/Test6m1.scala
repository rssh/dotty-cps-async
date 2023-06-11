package cpstest

import scala.annotation.experimental
import scala.concurrent.*
import scala.concurrent.duration.*
import scala.concurrent.ExecutionContext.Implicits.global

import cps.*
import cps.monads.{*,given}

@experimental
object Test6m1 {

  def fetch(url: String, errFlag: Boolean)(using CpsDirect[Future]): String =
    if (errFlag) {
      throw new RuntimeException(s"Error during fetch $url")
    } else {
      s"ok:$url"
    }

  def main(args:Array[String]): Unit = {

    val c = async[Future]{
      val r1 = fetch("url1",false)
      val r2 = fetch("url2",true)
      (r1+r2)
    }
    try {
      val r = Await.result(c, 1.second)
      println(r)
    } catch {
      case ex: RuntimeException =>
        println(ex.getMessage)
    }

  }

}
