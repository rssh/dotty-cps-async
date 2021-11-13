package cps.min1gopherlike

import org.junit.{Test,Ignore}
import org.junit.Assert._

import scala.quoted._
import scala.concurrent.Future
import scala.util.Success

import cps._
import cps.monads.FutureAsyncMonad
import scala.concurrent.ExecutionContext.Implicits.global

import cps.gopherlike._

class TestSF1W1:

  def qqq: Int = 0

  @Test def reproduce(): Unit = {
     //implicit val printCode = cps.macroFlags.PrintCode
     //implicit val printTree = cps.macroFlags.PrintTree
     //implicit val debugLevel = cps.macroFlags.DebugLevel(20)
     async[Future] {
        val ch1 = new CIFChannel[Future,Int]()
        val ch2 = new CIFChannel[Future,Int]()
        val select = SLSelectLoop.create[Future]
        //implicit val printCode = cps.macroFlags.PrintCode
        //implicit val debugLevel = cps.macroFlags.DebugLevel(20)
        for {
            _ <- Future.successful(())
            sf = select.afold((0, 0, 0)) { case ((n1, n2, nIdle), s) =>
                s.apply{
                    case x: ch1.read =>
                        val nn1 = n1 + 1
                        if (nn1 > 100) {
                            //s.done((nn1, n2, nIdle))
                            false
                        } else {
                            ch2.write(x)
                            true
                        }
                    case x: ch2.read =>
                        ch1.write(x)
                        //(n1, n2 + 1, nIdle)
                        true
                }
                SLSelectLoop.Done((n1, n2, nIdle))
            }
            r <- sf
        } yield r
       
     }
  }



