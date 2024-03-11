package vc

import scala.annotation.*
import scala.concurrent.*
import scala.concurrent.duration.*
import scala.concurrent.ExecutionContext.Implicits.global
import cmlib.NetworkAPI
import cps.*
import cps.monads.{*,given}


@experimental
object Main {

  def directFun(using CpsDirect[Future]): String = {
    val s = NetworkAPI.fetch("https://www.example.com")
    s
  }

  def main(args: Array[String]):Unit = {
    val f = async[Future] {
      val s = directFun
      println(s)
    }
    val fr = Await.ready(f,10.seconds)
  }

}
