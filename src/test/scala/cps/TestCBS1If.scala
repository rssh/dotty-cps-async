package cps

import org.junit.{Test,Ignore}
import org.junit.Assert._

import scala.quoted._
import scala.util.Success


class TestBS1If


  @Test def tIfC1_inBlock(): Unit = 
     val c = Async.transform[ComputationBound,Int]{
       { val x = await(T1.cbBool(true))
         val y = 3
         val z = if (x)
            await(T1.cbi(2))
         else 
            await(T1.cbi(3))
         y+z
       }
     }
     assert(c.run() == Success(5))

