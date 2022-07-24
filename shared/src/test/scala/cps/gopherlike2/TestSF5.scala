package cps.gopherlike2

import org.junit.{Test,Ignore}
import org.junit.Assert._

import scala.quoted._
import scala.concurrent.Future
import scala.util.Success

import cps._
import cps.monads.FutureAsyncMonad
import cps.testconfig.given
import scala.concurrent.ExecutionContext.Implicits.global


class TestSF5:

  def qqq: Int = 0

  @Test def reproduce(): Unit = {
     //implicit val printCode = cps.macroFlags.PrintCode
     //implicit val printTree = cps.macroFlags.PrintTree
     //implicit val debugLevel = cps.macroFlags.DebugLevel(20)

     val in = new CIFReader[Future,Boolean](true)
     val select = SLSelect[Future,Unit](summon[CpsMonad[Future]])

     // origin
     val generator = async[Future] {
       try 
           select.fold(in){ (chTestSF5,s) =>
              s.apply{
                case v: chTestSF5.read => chTestSF5
              }
           }
       catch
         case e: RuntimeException =>
           assert(e.getMessage() == "TestCase:runAsync:NotImplemented")
     }

     

     //  works
     //val generator = async[Future] {
     //     select.fold(in){ (ch,s) =>
     //       s.onRead(ch)(v => ch)
     //       SLSelect.Done(ch)
     //    }
     //}

     assert(true)

  }




