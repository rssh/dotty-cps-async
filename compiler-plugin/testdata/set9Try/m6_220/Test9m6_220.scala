package cpstest

import scala.annotation.experimental
import cps.*
import cps.monads.{*,given}
import cps.plugin.annotation.CpsDebugLevel
import testUtil.*



@experimental
@CpsDebugLevel(20)
object Test9m6_220 {

  var finallyWasRun=false

  def wrapped[A](a:A): CpsDirect[FreeMonad] ?=> A = a

  def simpleTry(x:String)(using CpsDirect[FreeMonad]): Int = {
    // TODO: pass lambda via adoptUcopsedCall, require changing adopt.
    //   (simpleTry will ave return type X=>X)
    //   (approx after initial version)
    ({
      try
        a => a + wrapped(x.toInt)
      catch
        case ex: NumberFormatException =>
          (a => wrapped(Integer.MIN_VALUE))
      finally
        finallyWasRun = true
    }: Int=>Int)(10)
  }

  def main(args:Array[String]): Unit = {
    val input = "10"
    val fr = reify[FreeMonad] {
      simpleTry(input)
    }
    val r = fr.eval
    if (r == 20 && finallyWasRun) then
      println("Ok")
    else
      println("r=$r")
  }


}