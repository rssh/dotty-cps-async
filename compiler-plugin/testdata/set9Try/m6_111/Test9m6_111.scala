package cpstest

import scala.annotation.experimental
import cps.*
import cps.monads.{*,given}

import testUtil.*


@experimental
object Test9m6_111 {

  var finallyWasRun=false

  def wrapped[A](a:A): CpsDirect[FreeMonad] ?=> A = a

  def simpleTry(x:String)(using CpsDirect[FreeMonad]): Either[String,Int] = {
    try
      Right(wrapped(x.toInt))
    catch
      case ex: NumberFormatException =>
        wrapped(Left(s"Invalid number $x"))
    finally
      finallyWasRun = wrapped(true)
  }

  def main(args:Array[String]): Unit = {
    val input = "10"
    val fr = reify[FreeMonad] {
      simpleTry(input)
    }
    val r = fr.eval
    if (r.isRight && finallyWasRun) then
      println("Ok")
    else
      println("r=$r")
  }


}