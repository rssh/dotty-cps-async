package cpstest

import scala.concurrent.*
import scala.concurrent.duration.*
import scala.concurrent.ExecutionContext.Implicits.global

import cps.*
import cps.monads.{ *, given }

object Test12m {

  def testAsyncFunc(): Future[String] = async[Future] {
    Thread.sleep(100)
    "Hello world"
  }

  def main(args: Array[String]): Unit =
    val res = Await.result(testAsyncFunc(), 1.seconds)
    println(res)
}
