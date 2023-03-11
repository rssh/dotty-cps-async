package cpstest

import scala.annotation.*
import cps.*

@experimental
object Example1 {

  def asyncPlus[F[_]](a:Int, b:F[Int]): CpsMonadContext[F] ?=> Int =
    a + cpsAwait(b)

  def contextPlus[F[_]](a: Int, b: CpsMonadContext[F] ?=> Int): CpsMonadContext[F] ?=> Int =
    a + b


}
  
