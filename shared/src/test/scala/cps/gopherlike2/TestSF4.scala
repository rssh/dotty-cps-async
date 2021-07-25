package cps.gopherlike2

import org.junit.{Test,Ignore}
import org.junit.Assert._

import scala.quoted._
import scala.concurrent.Future
import scala.util.Success

import cps._
import cps.monads.FutureAsyncMonad
import scala.concurrent.ExecutionContext.Implicits.global


class TestSF4:

  def qqq: Int = 0

  //compiler bug: https://github.com/lampepfl/dotty/issues/11401
  // fixed in https://github.com/lampepfl/dotty/pull/11552 (not yet in upstream)
  @Test def reproduce(): Unit = {
     //implicit val printCode = cps.macroFlags.PrintCode
     //implicit val printTree = cps.macroFlags.PrintTree
     //implicit val debugLevel = cps.macroFlags.DebugLevel(20)

     val in = new CIFReader[Future,Boolean](true)
     val select = SLSelect[Future,Unit](summon[CpsMonad[Future]])

     // origin
     //val generator = async[Future] {
     //    select.fold(in){ (ch,s) =>
     //       s.apply{
     //           case v: ch.read => ch
     //       }
     //    }
     //}

     
     val generator = async[Future] {
         try
           select.fold(in){ (ch,s) =>
              s.apply1(ch, v => ch)
           }
         catch
           case ex: RuntimeException =>
              assert(ex.getMessage() == "TestCase:runAsync:NotImplemented")
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




