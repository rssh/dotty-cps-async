package cpstest.s4.m2

import scala.annotation.experimental

import cps._
import cps.monads.{*,given}


@experimental
object Test4m2 {

  def direct1[A,F[_]](a:A): CpsDirect[F] ?=> A = a

  def illustrateMatch2_1[F[_]](x: String)(using CpsDirect[F]): Int = {
    direct1(x) match
      case "a" => 1
      case "b" => 2
      case "c" => 3
      case _ => -1
  }

  def illustrateMatch2_2[F[_]](x: String): (CpsDirect[F] ?=> Int) = {
    direct1(x) match
      case "a" => 1
      case "b" => 2
      case "c" => 3
      case _ => -1
  }

}
