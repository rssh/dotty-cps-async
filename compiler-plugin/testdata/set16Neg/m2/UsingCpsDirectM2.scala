package cpstest

import cps.*
import cps.monads.given

class UsingCpsDirectM2 {

  def one[F[_]](using CpsDirect[F]) = 1

  def shouldNotCompile1[F[_]](mc: CpsTryMonadContext[F]): Unit = {
    given CpsDirect[F] = CpsDirect(mc)
    val x = one
    println(x)
  }

}