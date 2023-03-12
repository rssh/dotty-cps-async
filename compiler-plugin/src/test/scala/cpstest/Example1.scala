package cpstest

import scala.annotation.*
import cps.*



@experimental
@cps.plugin.annotation.CpsDebugLevel(5)
object Example1 {


  def asyncPlus[F[_]](a:Int, b:F[Int]): CpsMonadContext[F] ?=> Int =
    a + cpsAwait(b)

  def contextPlus[F[_]](a: Int, b: CpsMonadContext[F] ?=> Int): CpsMonadContext[F] ?=> Int =
    a + b

  def contextPlus2[F[_]](a: Int, b: CpsMonadContext[F] ?=> Int)(using CpsMonadContext[F]): Int =
    a + b
  

}
  
