package cpstest

import cps.*
import cps.monads.{*,given}
import scala.concurrent.*
import scala.concurrent.duration.*
import scala.concurrent.ExecutionContext.Implicits.global

object Example2Inline {


  inline def asyncPlus2[F[_]](a0:Int,b0:F[Int])(using CpsMonadContext[F]): Int =
    a0 + await(b0)


  def main(args: Array[String]):Unit = {
      val fr = cpsAsync[Future]{
        asyncPlus2(1,(Future successful 2))
      }
      val r = Await.result(fr, 30.seconds)
      println(r)
  }

}