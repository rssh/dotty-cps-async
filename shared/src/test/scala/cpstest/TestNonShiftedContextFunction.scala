package cpstest

import scala.util.*

import cps.{*,given}
import cps.monads.{*, given}

import org.junit.{Test,Ignore}

class TestNonShiftedContextFunction {

  case class Context(x:String)


/*
 // disabled in lts
  object O {

    def apply[T](c:Context ?=> T): T = c(using Context("O"))

    def p0(x: => Int): Int = x + 1
    def p0_async(x:()  => ComputationBound[Int]): Int =
      x().run().get + 1

    def p1(x: Int=>Int): Int = x(1) + 1
    def p1_async(x: Int=>ComputationBound[Int]): Int =
      x(1).run().get + 1

  }

  @Test
  def testApplyContextFunctionP1(): Unit = {
      var x = 0
      val c = async[ComputationBound] {
          val a = await(T1.cbi(2))
          O {
            //val _ = ((x:Int) => x + await(T1.cbi(3)))
            val q = O.p1( x => x+ await(T1.cbi(3)) )
            if (false) then
              println(s"ctx=${summon[Context]}")
            a + q
          }
      }
      //println(s"run=${c.run()}")
      assert(c.run() == Success(7))
  }


  @Test
  def testApplyContextFunctionP0(): Unit = {
    var x = 0
    val c = async[ComputationBound] {
      val a = await(T1.cbi(2))
      O {
        //val _ = ((x:Int) => x + await(T1.cbi(3)))
        val q = O.p0(await(T1.cbi(3)))
        if (false) then
          println(s"ctx=${summon[Context]}")
        a + q
      }
    }
    assert(c.run() == Success(6))
  }
  */

}
