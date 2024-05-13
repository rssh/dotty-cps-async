package cpstest

import scala.annotation.experimental
import scala.concurrent.*
import scala.concurrent.duration.*
import scala.concurrent.ExecutionContext.Implicits.global

import cps.*
import cps.monads.{*,given}

object Hidded {
  val x = 1
}

@experimental
object Test7 {

  def direct[F[_],A](a:A)(using CpsDirect[F]):A = a

  def doSomethingWithHidder(using CpsDirect[Future]): Int =
    import Hidded.*
    x

  def main(args:Array[String]): Unit = {
      val fr = async[Future] {
        val a = direct(1)
        val b = doSomethingWithHidder
        a + b
      }
      val r = Await.result(fr,1.second)
      assert(r == 2)
      println(r)
  }

}