package cpstest

import scala.annotation.*
import cps.*



@experimental
@cps.plugin.annotation.CpsDebugLevel(15)
object Example1 {


  def asyncPlus[F[_]](a0:Int, b0:F[Int]): CpsDirect[F] ?=> Int =
    a0 + cpsAwait(b0)

  /*
  def contextPlus[F[_]](a1: Int, b1: CpsMonadContext[F] ?=> Int): CpsMonadContext[F] ?=> Int =
    a1 + b1

  def contextPlus2[F[_]](a2: Int, b2: CpsMonadContext[F] ?=> Int)(using CpsMonadContext[F]): Int =
    a2 + b2
  */

}
  
