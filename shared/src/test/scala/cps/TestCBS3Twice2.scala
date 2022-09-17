package cps

import org.junit.{Test,Ignore}
import org.junit.Assert.*

import scala.util.*
import cps.syntax.*
import cps.testconfig.given

class TestCBS3Twice2 {

   def twice2(x:Int, f: Int => Int):Int =
      f(f(x))

   def twice2Async(x: Int, f: Int => ComputationBound[Int]): ComputationBound[Int] =
      f(x).flatMap(f)   

   @Test def testTwice2(): Unit = 
      //implicit val printCode = cps.macros.flags.PrintCode
      //implicit val debugLevel = cps.macros.flags.DebugLevel(20)
      val c = async[ComputationBound]{
          val y = twice2(1,x => x + await(T1.cbi(x)))
          y
      }
      c.run() match 
          case Success(y) => assert( y == 4 )
          case _ => assert( "" == "shoud be seq from 4 elements") 
       

}