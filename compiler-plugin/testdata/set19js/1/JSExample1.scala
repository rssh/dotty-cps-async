package jsexample

import scalajs.js

import scala.concurrent.ExecutionContext.Implicits.global

object JSExample1 {

  //def makeAsyncString(msg: String): js.Promise[String] = {
  //  js.Promise.resolve(msg)
  //}

  def main(args: Array[String]): Unit = {
    println("Hello World form scalaJS")
    //val f = makeAsyncString("OK").toFuture.map {
    //  msg => println(msg)
    //}
  }

}
