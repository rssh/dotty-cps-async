package cpstest

import scala.annotation.*
import scala.concurrent.*
import scala.concurrent.duration.*
import scala.concurrent.ExecutionContext.Implicits.global

import cps.*
import cps.monads.{*,given}

@experimental
//@cps.plugin.annotation.CpsDebugLevel(15)
object Test5m1 {

  def fetch(url:String)(using CpsDirect[Future]): String =
    s"ok:$url"

  def testFun(): CpsDirect[Future] ?=> String = {
    var a = ""
    a = fetch("myurl")
    s"result:${a}"
  }

  def main(args: Array[String]): Unit = {
    val fr = async[Future] {
      testFun()
    }
    val r = Await.result(fr, 1000.millis)
    println(r)
  }

}