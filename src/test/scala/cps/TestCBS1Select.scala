package cps

import org.junit.{Test,Ignore}
import org.junit.Assert._

import scala.quoted._
import scala.util.Success


class TestCBS1Select:

  case class P(val x:Int, val y:Int)

  object PD {
     def fx(x:Int):Int = x+1
     def fy(y:Int):Int = y+2

  }

  @Test def sel_field0(): Unit = 
     //implicit val printCode = cps.macroFlags.PrintCode
     //implicit val debugLevel = cps.macroFlags.DebugLevel(20)
     val c = async{
       val p = P(3,4)
       p.y
     }
     assert(c.run() == Success(4))
  

  @Test def sel_fun_w0(): Unit = 
     val c = async{
       (if (true) PD.fx else PD.fy)(0)
     }
     assert(c.run() == Success(1))

  @Test def sel_fun_0(): Unit = 
     val c = async{
       PD.fx
     }
     assert(c.run().isSuccess)
     val c2 = c.map(x => x(1))
     assert(c2.run() == Success(2))


  @Test def sel_out_0(): Unit = 
     object O1 {
          val k: Int = 3
          object O2 {
            val q = async{
                   O1.k
            }
         }
     }
     val c = O1.O2.q
     assert(c.run().isSuccess)


