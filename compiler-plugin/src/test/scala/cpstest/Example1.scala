package cpstest

import scala.annotation.*
import cps.*

@experimental
object Example1 {
  import cps.E.*

  def asyncPlus[F[_]](a:Int, b:F[Int]): CpsTransform[F] ?=> Int =
    a + cpsAwait(b)
  

}
  
