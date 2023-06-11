package cpstest.s4.m3

import scala.annotation.experimental
import cps._
import cps.monads.{*,given}
import testUtil.*


@experimental
object Test4m3 {

  def direct1[A, F[_]](a: A): CpsDirect[F] ?=> A = a

  def illustrateMatch3_1[F[_]](x: String)(using CpsDirect[F]): Int = {
    direct1(x) match
      case "a" => 1
      case "b" => direct1(2)
      case "c" => 3
      case _ => -1
  }
  
  def main(args:Array[String]):Unit = {
    val f = reify[FreeMonad] {
      illustrateMatch3_1("a")
    }

    val r = f.eval
    println(r)
  }

}
