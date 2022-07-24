package cps.gopherlike

import org.junit.{Test,Ignore}
import org.junit.Assert._

import scala.quoted._
import scala.concurrent.Future
import scala.util.Success

import cps._
import cps.monads.FutureAsyncMonad
import cps.testconfig.given
import scala.concurrent.ExecutionContext.Implicits.global


class TestSL3:

  def qqq: Int = 0

  @Test def reproduce(): Unit = 
     //implicit val printCode = cps.macroFlags.PrintCode
     //implicit val printTree = cps.macroFlags.PrintTree
     //implicit val debugLevel = cps.macroFlags.DebugLevel(20)
     val info = new CIFChannel[Future,Long]()
     val quit = new CIFChannel[Future,Int]()

     var (x,y) = (0L,1L)

     var sum = 0L
     //implicit val printCode = cps.macroFlags.PrintCode
     //implicit val debugLevel = cps.macroFlags.DebugLevel(20)

     val reader = async[Future] {
         SLSelectLoop.create[Future].apply{
            case z: info.read => //sum += z
                              //if (sum > 10000) {
                                 //quit.write(1)
                                 await(quit.awrite(1))
                                 false
                              //} else {
                              //   true
                              //}
         }
     }
