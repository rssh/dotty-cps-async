package cpstest

import scala.annotation.experimental
import cps.*
import cps.monads.{*,given}
import cps.plugin.annotation.CpsDebugLevel

import testUtil.*

//@CpsDebugLevel(20)
@experimental
object Test9m3 {

  def wrapped[A](a:A): CpsDirect[FreeMonad] ?=> A = a

  def simpleTry(x:String)(using CpsDirect[FreeMonad]):Either[String,Int] = {
    try
      Right(wrapped(x).toInt)
    catch
      case ex:NumberFormatException =>
        Left(s"Invalid number format ${wrapped(x)}")

  }

  def main(args:Array[String]): Unit = {
    val fr = reify[FreeMonad] {
      simpleTry("10")
    }
    val r = fr.eval
    if (r==Right(10)) then
      println("Ok")
    else
      println(s"r=$r")
  }


}