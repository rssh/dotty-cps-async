package cpstest

import scala.annotation.experimental
import scala.concurrent.*
import scala.concurrent.duration.*
import scala.concurrent.ExecutionContext.Implicits.global

import cps.*
import cps.monads.{ *, given }

@experimental
object Test8m1 {
  
  def syncLoop(url: String)(using CpsDirect[Future]): String =
    var i = 0
    while (i < 3) do {
      i += 1
    }
    s"$url:$i"

  def main(args: Array[String]): Unit = {
    val fr = async[Future] {
      syncLoop("myurl")
    }
    val r  = Await.result(fr, 1000.millis)
    println(r)
  }

}
