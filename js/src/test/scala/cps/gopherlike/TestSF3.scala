package cps.gopherlike

import org.junit.{Test,Ignore}
import org.junit.Assert._

import scala.quoted._
import scala.concurrent.Future
import scala.util.Success

import cps._
import cps.monads.FutureAsyncMonad
import scala.concurrent.ExecutionContext.Implicits.global

import cps.testconfig.given

class TestSF3D:

  def qqq: Int = 0

  /* 
  // js bug: endless execution.  TODO: localize
  @Test def reproduce(): Future[Unit] = {
     //implicit val printCode = cps.macroFlags.PrintCode
     //implicit val printTree = cps.macroFlags.PrintTree
     //implicit val debugLevel = cps.macroFlags.DebugLevel(20)

     
     val ch = new CIFChannel[Future,Boolean]()
     val chDone = new CIFChannel[Future,Boolean]()
     val select = SLSelectLoop[Future]

     val sf = select.afold((true)){ (x,s) =>
            println(s"in afold, s=$s, x=$x")
            s.apply{
                case v: ch.read => 
                    x || v
                //case v: chDone.read => v
            }
            x
         }
   
     sf.map(x => assert(true))
  }
  */
  



