package cpstest

import scala.annotation.*
import scala.concurrent.*
import scala.concurrent.duration.*
import scala.concurrent.ExecutionContext.Implicits.global

import cps.*
import cps.monads.{*,given}


//@cps.plugin.annotation.CpsDebugLevel(15)
//TODO:  the samw with some code in testFun after assingment.
@experimental
object Test5m2 {

  class X{
    var value: String = "__INIT__"
  }

  def fetch(url:String)(using CpsDirect[Future]): String =
    s"ok:$url"

  def testFun(x:X): CpsDirect[Future] ?=> Unit = {
    x.value = fetch("myurl")
  }

  def main(args: Array[String]): Unit = {
    val x = new X()
    val fr = async[Future] {
      testFun(x)
    }
    val r = Await.result(fr, 1000.millis)
    println(x.value)
  }

}