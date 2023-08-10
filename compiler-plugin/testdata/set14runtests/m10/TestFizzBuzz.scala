package cps.pe

import org.junit.{Test,Ignore}
import org.junit.Assert.*

import scala.util.*


import scala.concurrent.*
import scala.concurrent.duration.*

import cps.*
import cps.automaticColoring.given
import cps.plugin.annotation.CpsDebugLevel
import scala.language.implicitConversions


import cps.testconfig.given

import scala.concurrent.ExecutionContext.Implicits.global

// This test will be deleted after disabling of automatic coloring.
//@CpsDebugLevel(20)
class TestFizzBuzz:


  //implicit val printCode: cps.macroFlags.PrintCode.type = cps.macroFlags.PrintCode
  //implicit val printTree = cps.macroFlags.PrintTree
  //implicit inline def debugLevel: cps.macroFlags.DebugLevel = cps.macroFlags.DebugLevel(10)


  @Test def testFizBuzz = 
     val c = async[PureEffect] {
       val logger = PEToyLogger.make()
       val counter = PEIntRef.make(-1)
       println(s"crrate counter, value=${counter.value}")
       while {
         val v = counter.increment()
         logger.log(await(v).toString)
         if (v % 3 == 0) then
            logger.log("Fizz")
         if (v % 5 == 0) then
            logger.log("Buzz")
         v < 10
       } do ()
       await(logger.all())
     }
     println(s"PE:fizbuzz, c=${c} ")
     val future = c.unsafeRunFuture().map{ log =>
       println(s"PE:fizbuzz, log=${log} ")
       assertTrue(log(0)=="0")
       assertTrue(log(1)=="Fizz")
       assertTrue(log(2)=="Buzz")
       assertTrue(log(3)=="1")
       assertTrue(log(4)=="2")
       assertTrue(log(5)=="3")
       assertTrue(log(6)=="Fizz")
       assertTrue(log(7)=="4")
     }
     val r = Await.result(future,1.second)




