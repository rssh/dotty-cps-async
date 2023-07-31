package cpstest

import scala.concurrent.*
import scala.concurrent.duration.*
import scala.concurrent.ExecutionContext.Implicits.global

import cps.*
import cps.monads.{ *, given }

object Test12m3 {

  @cps.plugin.annotation.makeCPS
  def getUrlBuilder(c: String): String => String =
    path => c + path

  def main(args: Array[String]): Unit = {
    val fr = async[Future] {
      val buildUrl = getUrlBuilder("myurl")
      buildUrl("/item")
    }
    val r  = Await.result(fr, 1000.millis)
    println(r)
  }
}
