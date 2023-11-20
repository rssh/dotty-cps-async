package cpstest

import cps.*
import cps.monads.{*,given}
import scala.annotation.*
import scala.concurrent.*
import scala.concurrent.duration.*
import scala.concurrent.ExecutionContext.Implicits.global

@experimental
//@cps.plugin.annotation.CpsDebugLevel(15)
object Example2Inline {


  inline def asyncPlus2[F[_]](a0:Int,b0:F[Int])(using CpsDirect[F]): Int =
    a0 + await(b0)

  inline def asyncPlus3[F[_]](a0:Int,b0:F[Int]): CpsDirect[F] ?=> Int =
    a0 + await(b0)

  def main(args: Array[String]):Unit = {
      val fr2 = cpsAsync[Future]{
        asyncPlus2(1,(Future successful 2))
      }
      val r2 = Await.result(fr2, 30.seconds)
      println(r2)
      //pending:
      val fr3 = cpsAsync[Future]{
        asyncPlus3(1,(Future successful 2))
      }
      val r3 = Await.result(fr3, 30.seconds)
      println(r3)
  }

}