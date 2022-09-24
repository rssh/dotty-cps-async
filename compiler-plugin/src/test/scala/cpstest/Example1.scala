package cpstest

import cps.*


object Example1 {

  def asyncPlus[F[_]](a:Int, b:F[Int])(using cps: CpsTransform[F]): { cps } Int =
    a + cps.await(b)
  

}
  
