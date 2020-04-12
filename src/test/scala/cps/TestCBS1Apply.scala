package cps

import org.junit.{Test,Ignore}
import org.junit.Assert._

import scala.quoted._
import scala.util.Success


class TestCBS1Apply:



  @Test def apply_fun1(): Unit = {
     //implicit val printCode = cps.macroFlags.PrintCode
     //implicit val debugLevel = cps.macroFlags.DebugLevel(20)
     val c = async{
       await(T1.cbi(2)) + 3
     }
     assert(c.run() == Success(5))
  }

  class Zzz(x:Int) {
     def ta[T](t:T):String =
          t.toString + x.toString
  }

  @Test def apply_fun2(): Unit = {
     //implicit val printCode = cps.macroFlags.PrintCode
     //implicit val debugLevel = cps.macroFlags.DebugLevel(20)
     val c = async{
       await(T1.cbi(2).map(new Zzz(_))).ta("qqq")
     }
     assert(c.run() == Success("qqq2"))
  }


