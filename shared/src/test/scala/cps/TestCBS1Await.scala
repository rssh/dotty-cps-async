package cps

import org.junit.{Test,Ignore}
import org.junit.Assert._

import scala.quoted._
import scala.util.Success
import cps.testconfig.given

class TestCBS1Await:

  @Test def apply_fun_await_1_0(): Unit = {
     //implicit val printCode = cps.macroFlags.PrintCode
     //implicit val debugLevel = cps.macroFlags.DebugLevel(20)
     val c = async{
       val fun = T1.cbt[Int=>Int](x => x+1)
       (await(fun))(1)
     }
     assert(c.run() == Success(2))
  }

  @Test def apply_fun_await_1_1(): Unit = {
     //implicit val printCode = cps.macroFlags.PrintCode
     //implicit val debugLevel = cps.macroFlags.DebugLevel(20)
     val c = async{
       val fun = T1.cbt[Int=>Int](x => x+1)
       val a = T1.cbi(1)
       (await(fun))(await(a))
     }
     assert(c.run() == Success(2))
  }
  

  @Test def apply_fun_await_0_1(): Unit = {
     //implicit val printCode = cps.macroFlags.PrintCode
     //implicit val debugLevel = cps.macroFlags.DebugLevel(20)
     val c = async{
       val fun: Int=>Int = (x:Int) => x+1
       val a = T1.cbi(1)
       fun(await(a))
     }
     assert(c.run() == Success(2))
  }
  

  @Test def apply_fun_await_0_11(): Unit = {
     //implicit val printCode = cps.macroFlags.PrintCode
     //implicit val debugLevel = cps.macroFlags.DebugLevel(20)
     def fun(x:Int,y:Int):Int = x+y
     val c = async{
       val a = T1.cbi(1)
       val b = T1.cbi(2)
       fun(await(a), await(b))
     }
     assert(c.run() == Success(3))
  }
  
  @Test def apply_fun_await_0_0_1(): Unit = {
     //implicit val printCode = cps.macroFlags.PrintCode
     //implicit val debugLevel = cps.macroFlags.DebugLevel(20)
     def fun(x:Int):ComputationBound[Int] =  T1.cbi(x+1)
     val c = async{
       val x:Int = await(fun(2))
       x
     }
     assert(c.run() == Success(3))
  }
  

  @Test def apply_fun_await_0_1_1(): Unit = {
     //implicit val printCode = cps.macroFlags.PrintCode
     //implicit val debugLevel = cps.macroFlags.DebugLevel(20)
     def fun(x:Int):ComputationBound[Int] =  T1.cbi(x+1)
     val c = async{
       val x:Int = await(fun(await(T1.cbi(2))))
       x
     }
     assert(c.run() == Success(3))
  }
  

  @Test def nested_await_3(): Unit = {
     //implicit val printCode = cps.macros.flags.PrintCode
     //implicit val debugLevel = cps.macros.flags.DebugLevel(20)
     val c = async{
       val fffx = T1.cbt(T1.cbt(T1.cbi(2)))
       val x:Int = await(await(await(fffx))) + 1
       x
     }
     assert(c.run() == Success(3))
  }
  

