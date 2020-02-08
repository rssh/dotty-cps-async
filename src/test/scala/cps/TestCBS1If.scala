package cps

import org.junit.{Test,Ignore}
import org.junit.Assert._

import scala.quoted._
import scala.util.Success

class TestBS1If

/*
  @Test def tIfC1_inBlock(): Unit = 
     val c = Async.transform[ComputationBound,Int]{
       { val x = await(T1.cbi(3))
         val y = 3
         val z = await(T1.cbi(2))
         //y+z
         y+z
       }
     }
     assert(c.run() == Success(5))
*/

  @Test def tBadTree(): Unit = 
     val c = B.badTree[Int]{
       { val x :scala.Boolean = await(T1.cbBool(true))
         val y :scala.Int = 3
         val z :scala.Int = await(T1.cbi(2))
         y+z
       }
     }
     assert(c.run() == Success(5))


