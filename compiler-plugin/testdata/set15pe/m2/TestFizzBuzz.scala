package cps.pe

import org.junit.{Test,Ignore}
import org.junit.Assert.*

import scala.util.*


import scala.annotation.experimental
import scala.concurrent.*
import scala.concurrent.duration.*

import cps.*
import scala.concurrent.ExecutionContext.Implicits.global

@experimental
class TestFizzBuzz:

  import cps.pe.localDirectRef.*


  @Test def testFizBuzz =
    val c = async[PureEffect] {
      val logger = PEToyLogger.make()
      val counter = PureEffect.localDirectIntRef(-1)
      while {
        val v = counter.increment()
        logger.log(v.toString)
        if (v % 3 == 0) then
          logger.log("Fizz")
        if (v % 5 == 0) then
          logger.log("Buzz")
        v < 10
      } do ()
      logger.all()
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

