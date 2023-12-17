package jsexample

import scalajs.js

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import cps.*
import cps.monads.{*,given}


object JSExample1 {

  def makeAsyncString(msg: String): js.Promise[String] = {
    js.Promise.resolve(msg)
  }

  def main(): Unit = {
    //println("Hello World form scalaJS")
    //val f = makeAsyncString("OK").toFuture.map {
    //  msg => println(msg)
    //}
    val f = async[Future] {
      val msg1 = await(makeAsyncString("String from Promise"))
      println(msg1)
    }
    f
  }

}
