package cpstest

import scala.annotation.experimental
import cps.*
import cps.monads.{*,given}
import cps.plugin.annotation.CpsDebugLevel

import testUtil.*

//@CpsDebugLevel(20)
@experimental
object Test10m1 {

  def fun1(x:String)(using CpsDirect[FreeMonad]): Int = {
     x.length
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