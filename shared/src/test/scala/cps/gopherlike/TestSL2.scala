package cps.gopherlike

import org.junit.{Test,Ignore}
import org.junit.Assert._

import scala.quoted._
import scala.concurrent.Future
import scala.util.Success

import cps._
import cps.monads.FutureAsyncMonad
import scala.concurrent.ExecutionContext.Implicits.global


class TestSL2:

  def qqq = 1

  @Test def reproduce(): Unit = 
     //implicit val printCode = cps.macroFlags.PrintCode
     //implicit val printTree = cps.macroFlags.PrintTree
     //implicit val debugLevel = cps.macroFlags.DebugLevel(20)
     async[Future] {

        val channel1 = CIFChannel[Future,Int]()
        val channel2 = CIFChannel[Future,Int]()
        val channel3 = CIFChannel[Future,Int]()
        val channel4 = CIFChannel[Future,Int]()


        val q = 0
        val selector = SLSelectLoop.create[Future].runAsync()

        for(c <- channel4) 
           channel2.write(c)
        
        await(selector)

        assert(q == 1)


     }

