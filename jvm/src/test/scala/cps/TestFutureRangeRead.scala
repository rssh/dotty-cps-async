package cps

import cps.testconfig.given

import org.junit.{Test,Ignore}
import org.junit.Assert._

import scala.concurrent._
import scala.concurrent.duration._
import scala.language.postfixOps
import scala.quoted._
import scala.util.Success

import java.util.concurrent.ForkJoinPool


class TestFutureRangeReader:

  //import scala.concurrent.ExecutionContext.Implicits.global 

  given ExecutionContext = ExecutionContext.fromExecutorService(ForkJoinPool.commonPool().nn)

  import cps.monads.{*,given}
 

  @Test def tryFewRangeLoops(): Unit = 
     val ch1 = new ASChannel[Future,Int]()
     val ch2 = new ASChannel[Future,Int]()

     val loop1 = async[Future] {
        for(i <- 1 to Int.MaxValue) {
           await(ch1.write(i))
        } 
     }

     val loop2 = async[Future] {
        for(i <- 1 to Int.MaxValue) {
           val r = await(ch1.read())
           if (r % 2 == 0) {
              await(ch2.write(r))
           }
        }
     }
     
     val loop3 = async[Future] {
        for(i <- 1 to 100) yield {
          val r = await(ch2.read())
          r
        }
     }


     val r = Await.result(loop3, 10 seconds)
     assert(r(0)==2)
     assert(r(1)==4)


