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


class TestSF3:

  def qqq: Int = 0

  // dotty bug: https://github.com/lampepfl/dotty/issues/11251
  @Test def reproduce(): Unit = {
     //implicit val printCode = cps.macroFlags.PrintCode
     //implicit val printTree = cps.macroFlags.PrintTree
     //implicit val debugLevel = cps.macroFlags.DebugLevel(20)

     
     val ch = new CIFChannel[Future,Boolean]()
     val chDone = new CIFChannel[Future,Boolean]()
     val select = SLSelectLoop.create[Future]

     val sf = select.afold((true)){ (x,s) =>
            
            s.apply{
                case v: ch.read => 
                    x || v
                //case v: chDone.read => v
            }
            x
         }
   
     assert(true)
  }
  



