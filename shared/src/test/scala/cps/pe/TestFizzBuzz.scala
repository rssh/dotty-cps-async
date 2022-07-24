package cps.pe

import org.junit.{Test,Ignore}
import org.junit.Assert.*

import scala.util.*

import cps.*
import cps.automaticColoring.given
import cps.util.FutureCompleter
import scala.language.implicitConversions

import cps.testconfig.given

import scala.concurrent.ExecutionContext.Implicits.global


class TestFizzBuzz:


  //implicit val printCode: cps.macroFlags.PrintCode.type = cps.macroFlags.PrintCode
  //implicit val printTree = cps.macroFlags.PrintTree
  //implicit inline def debugLevel: cps.macroFlags.DebugLevel = cps.macroFlags.DebugLevel(10)


  @Test def testFizBuzz = 
     val c = async[PureEffect] {
       val logger = PEToyLogger.make()
       val counter = PEIntRef.make(-1)
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
     val future = c.unsafeRunFuture().map{ log =>
       //println(s"PE:fizbuzz, log=${log} ")
       assert(log(0)=="0")
       assert(log(1)=="Fizz")
       assert(log(2)=="Buzz")
       assert(log(3)=="1")
       assert(log(4)=="2")
       assert(log(5)=="3")
       assert(log(6)=="Fizz")
       assert(log(7)=="4")
     }
     FutureCompleter(future)




