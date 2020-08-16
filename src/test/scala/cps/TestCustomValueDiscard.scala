package cps

import org.junit.{Test,Ignore}
import org.junit.Assert._

import scala.concurrent._
import scala.concurrent.duration._
import scala.language.postfixOps
import scala.language.implicitConversions
import scala.quoted._
import scala.util.Success

import cps.features.customValueDiscard.{_, given _}
import cps.features.ValueDiscard

class TestCustomValueDiscard:

  import scala.concurrent.ExecutionContext.Implicits.global 

  case class MyObj(value:Int)
  

  @Test def withCustomValueDiscard(): Unit = 
     //implicit val printCode = cps.macroFlags.PrintCode
     //implicit val printTree = cps.macroFlags.PrintTree
     //implicit val debugLevel = cps.macroFlags.DebugLevel(10)
     
     var x = 0
     given ValueDiscard[MyObj] {
        override def apply(value:MyObj) = 
           x += value.value
     }

     val c = async[ComputationBound]{ 
         await(T1.cbt(MyObj(1)))
         await(T1.cbt(MyObj(2)))
     }
     val r = c.run()
     assert(r.isSuccess)
     assert(x == 1)

     x = 0
     val c1 = async[ComputationBound]{ 
         await(T1.cbt(MyObj(1)))
         await(T1.cbt(MyObj(2)))
         ()
     }
     val r1 = c1.run()
     assert(r1.isSuccess)
     assert(x == 3)



