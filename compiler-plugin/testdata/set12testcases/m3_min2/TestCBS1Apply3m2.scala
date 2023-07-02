package cps

import org.junit.{Test,Ignore}
import org.junit.Assert._

import scala.quoted._
import scala.util.Success


import cps.syntax.*
import cps.macros.flags.*
import cps.plugin.annotation.CpsDebugLevel
//import cps.testconfig.given
given UseCompilerPlugin.type = UseCompilerPlugin



//@CpsDebugLevel(20)
class TestCBS1Apply3m2:



  class DifferentAsync[F[_]:CpsMonad](x: F[Int]) {

    def fvalue: F[Int] = x

    def map(f: Int=>Int): DifferentAsync[F] =
      DifferentAsync(summon[CpsMonad[F]].map(x)(f))

    def mapAsync(f: Int=>F[Int]): DifferentAsync[F] =
      DifferentAsync(summon[CpsMonad[F]].flatMap(x)(f))


  }


  @Test def apply_mapAsyncSame(): Unit = {
    //implicit val printCode = cps.macroFlags.PrintCode
    //implicit val debugLevel = cps.macroFlags.DebugLevel(20)
    val c = async {
      val d1 = DifferentAsync[ComputationBound](T1.cbi(2))
      //val d2 = d1.map(x => x+1)
      val d3 = d1.map(x => x+await(T1.cbi(2)))
      await(d3.fvalue)
    }
    assert(c.run() == Success(4))

  }



