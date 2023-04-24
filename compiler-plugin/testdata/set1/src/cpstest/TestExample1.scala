package cpstest


import scala.annotation.*
import scala.concurrent.*
import scala.concurrent.duration.*
import scala.util.*

import org.junit.{Test,Ignore}
import org.junit.Assert._


import cps.*

import cps.monads.{*,given}
import cps.plugin.annotation.CpsDebugLevel


@experimental
class TestExample1:

  
  @CpsDebugLevel(1)
  @Test def testExample1Future(): Unit = 
     import scala.concurrent.ExecutionContext.Implicits.global
     
     val fr = cpsAsync[Future] {
        val y = Example1.asyncPlus(1,Future successful 2)
        y+1
     };

     val r = Await.result(fr, 30.seconds)
     assert(r == 4)

  def myFun(): Unit = {
    println(1)
  }

object TestExample1:

  def main(args: Array[String]):Unit = {
    val t = new TestExample1()
    t.testExample1Future()
    println("Ok")
  }

end TestExample1

  /*
  @Test def testExample1Free(): Unit = 
     val fr = cpsAsync[Free] {
        val y = Example1.asyncPlus(1, Free.pure(2))
        y+1
     }
     // TODO: pure.eval
     println("Free representation:"+fr)
     */


