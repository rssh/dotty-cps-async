package cpstest

import cps.*
import cps.monads.given

class TryToUseCpsDirectM1 {

  def one[F[_]](using CpsDirect[F]) = 1

  def shouldNotCompile1[F[_]](mc: CpsTryMonadContext[F]): Unit = {
    implicit val direct = CpsDirect(mc)
    val x = one
    println(x)
  }

}