package cpstest.s4.m1

import cps._
import cps.monads.{*,given}
import scala.annotation.experimental

@experimental
object Test4m1 {

  def pure2[F[_]](x:Int)(using CpsDirect[F]): Int =
    await(summon[CpsDirect[F]].monad.pure(x))

  def illustrateMatch1[F[_]](x:String)(using CpsDirect[F]): Int = {
    x match
        case "a" => await(summon[CpsDirect[F]].monad.pure(1))
        case "b" => 2
        case "c" => pure2(3)
        case _ => -1
  }

  /*
  def eval[A](fa:FreeMonad[A]):A =
    fa match
      case FreeMonad.Pure(a) => a
      case FreeMonad.FlatMap(a,f) => eval(f(eval(a)))
      case FreeMonad.Error(e) => throw e
      case FreeMonad.FlatMapTry(a,f) =>
        try{
          eval(a)
        }
  */

  def main(args:Array[String]): Unit =
     import scala.concurrent.ExecutionContext.Implicits.global
     val fr = reify[FreeMonad] {
        val r = illustrateMatch1[FreeMonad]("a")
        println(s"r=$r")
     }
}

