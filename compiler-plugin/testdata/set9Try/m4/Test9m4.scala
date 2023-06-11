package cpstest

import scala.annotation.experimental
import cps.*
import cps.monads.{*,given}
import cps.plugin.annotation.CpsDebugLevel

import testUtil.*

//@CpsDebugLevel(20)
@experimental
object Test9m4 {

  def wrapped[A](a:A): CpsDirect[FreeMonad] ?=> A = a

  def simpleTry(x:String)(using CpsDirect[FreeMonad]):Either[String,Int] = {
    try
      Right(wrapped(x).toInt)
    catch
      case ex:NumberFormatException =>
        Left(s"Invalid number format ${wrapped(x)}")

  }

  def main(args:Array[String]): Unit = {
    val input = "x-not-a-number-xxx"
    val fr = reify[FreeMonad] {
      simpleTry(input)
    }
    val r = fr.eval
    r match
      case Right(x) =>
        println("?? x=$x")
      case Left(message) =>
        if message == s"Invalid number format $input" then
           println("Ok")
        else
           println("?? message=$message")
  }


}