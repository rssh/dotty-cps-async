package cpstest

import scala.concurrent.*
import scala.concurrent.duration.*
import scala.concurrent.ExecutionContext.Implicits.global

import cps.*
import cps.monads.{ *, given }

object Test8m1 {
  
  def fetch(url: String)(using CpsDirect[Future]): String =
    var i = 0
    while (i < 3) do {
      i += 1
    }
    s"$url:$i"

  def main(args: Array[String]): Unit = {
    val fr = async[Future] {
      fetch("myurl")
    }
    val r  = Await.result(fr, 1000.millis)
    println(r)
  }

}
