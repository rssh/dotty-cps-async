package cpstest

import scala.annotation.experimental
import cps.*
import cps.monads.{*,given}
import cps.plugin.annotation.CpsDebugLevel

import testUtil.*

@experimental
@CpsDebugLevel(20)
object Test10m2 {

  def direct(x:String)(using CpsDirect[FreeMonad]): String = x

  def fun1(x:String)(using CpsDirect[FreeMonad]): Int = {
    direct(x).length
  }

  def main(args:Array[String]): Unit = {
    val fr = reify[FreeMonad] {
      fun1("aaa")
    }
    val r = fr.eval
    if (r==3) {
      println("Ok")
    } else {
      println(r)
    }
  }

}