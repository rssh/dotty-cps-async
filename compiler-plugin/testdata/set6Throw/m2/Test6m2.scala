package cpstest

import scala.annotation.experimental
import scala.concurrent.*
import scala.concurrent.duration.*
import scala.concurrent.ExecutionContext.Implicits.global

import cps.*
import cps.monads.{*,given}


@experimental
object Test6m2 {

  def directMessage(using CpsDirect[Future]): String =
    "Hello"

  def raiesException(errFlag:Boolean)(using CpsDirect[Future]): String =
    if (errFlag) {
      throw new RuntimeException(directMessage)
    } else {
      "ok"
    }

  def main(args:Array[String]): Unit = {

    val c = async[Future]{
      raiesException(true)
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
