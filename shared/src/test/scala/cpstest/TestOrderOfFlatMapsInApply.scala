package cpstest

import org.junit.{Test,Ignore}
import org.junit.Assert._

import scala.util.*


import cps.*
import cps.monads.{*,given}

import cps.testconfig.given
import cps.util.FutureCompleter



class TestOrderOfFlatMapsInApply {


     //@Test
     //def orderOfFlatMapsFuture() {
     //
     //}


 
   @Test
   def orderOfFlatMapsList() = {
        val x = List(1, -2, -3)
        val y = List("ab", "cde")

        val r1 = reify[List] {
           val xx = reflect(x)
           reflect(y).length * reflect(x)
        }
     
        val r2 = x.flatMap{ x1 =>
           val xx = x1
           y.flatMap{ y1 =>
              x.map{ x2 =>
                 y1.length * x2
              }
           }
        }

        //println(s"r1=$r1,  r2=$r2")
        assert(r1 == r2)
   }


   @Test
   def testMinSeq() = {

      //implicit val printCode = cps.macros.flags.PrintCode
      //implicit val debugLevel = cps.macros.flags.DebugLevel(20)


      val x = List(1,2)
      val y = List(1,-1)

      val r1 = reify[List]{
         reflect(x)+reflect(y)
      }

      val r2 = x.flatMap{ x1 =>
         y.map{ y1 =>  x1+y1 }
      }
      // List((1+1),(1+-1),(2+1),(2-1)) = List(2,0,3,1)

      val r3 = y.flatMap{ y1 =>
         x.map{ x1 =>  x1+y1 }
      }
      // (1+1, 1+2, -1+1, -1+2) = List(2,3,0,1)

      //println(s"r1=$r1,  r2=$r2, r3=$r3")
      assert(r1 == r2)
   }

}
