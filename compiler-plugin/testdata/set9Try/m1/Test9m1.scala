package cpstest

import scala.annotation.experimental
import cps.*
import cps.monads.{*,given}

import testUtil.*

@experimental
object Test9m1 {

  def simpleTry(x:String)(using CpsDirect[FreeMonad]):Either[String,Int] = {
    try
      Right(x.toInt)
    catch
      case ex:NumberFormatException =>
        Left(s"Invalid number format ${x}")

  }

  def main(args:Array[String]): Unit = {
    val fr = reify[FreeMonad] {
      simpleTry("10")
    }
    val r = fr.eval
    println(r)
  }


}