package cpstest

import scala.annotation.*
import scala.concurrent.*
import scala.concurrent.duration.*
import scala.concurrent.ExecutionContext.Implicits.global

import cps.*
import cps.monads.{ *, given }

object Test12m1 {

  @cps.plugin.annotation.makeCPS
  def fetchConst(c: String): String = c

  def main(args: Array[String]): Unit = {
    val fr = async[Future] {
      fetchConst("myurl")
    }
    val r  = Await.result(fr, 1000.millis)
    println(r)
  }
}
