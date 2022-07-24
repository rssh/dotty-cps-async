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


class TestSF2:

  def qqq: Int = 0

  case class State(var sum: Int = 0)

/*
  @Test def reproduce(): Unit = {
     //implicit val printCode = cps.macroFlags.PrintCode
     //implicit val printTree = cps.macroFlags.PrintTree
     //implicit val debugLevel = cps.macroFlags.DebugLevel(20)
     val ch1 = new CIFChannel[Future,Int]()
     val ch2 = new CIFChannel[Future,Int]()
     val select = SLSelectLoop[Future]
     var state = new State
     val out = select.map1{s =>
             s.apply{
                case x: ch1.read => 
                           state.sum = state.sum + x*1 
                           true
                case x: ch2.read => 
                           false
             }
             state
     } 
     assert(true)
  }
*/




