package cpstest

import org.junit.{Test,Ignore}
import org.junit.Assert.*

import scala.concurrent.*
import scala.concurrent.duration.*
import scala.concurrent.ExecutionContext.Implicits.global

import cps.*
import cps.monads.{*,given}
import cps.stream.*

import cps.util.*
import cps.testconfig.given


class TestAsyncListStress:

    
    @Test @Ignore  def testMergeKNonEmptyTimedLists() = {
    
      val nRuns = 10
      val nStreams = 10
      val delays = Array(50,100)
      val streamSize = 1000
  

      for( i <- 1 to nRuns)  {

        val f = async[Future] {

            var mergedStream: AsyncList[Future,Int] = AsyncList.empty[Future]
            for (j <- 1 to nStreams) {
               val stream = asyncStream[AsyncList[Future,Int]] { out =>
                    val delay = delays( j % delays.length ).milliseconds
                    for(i <- 1 to streamSize) {
                       await(FutureSleep(delay))
                       out.emit(i)
                    }
               }
               mergedStream = mergedStream.merge(stream)
            }

            val fAll = mergedStream.takeListAll()
            val all = await(fAll)
            assert(all.size == nStreams * streamSize)
            println(s"TestAsyncListMergeStress: done $i")
            
        }

        val limit = (delays.max * streamSize * 3).milliseconds
        println(s"TestAsycListMetgeStress: waiting ${limit}  = ${limit.toSeconds} seconds")
        Await.result(f, limit)

      }

    }

    @Test @Ignore  def testTimedGenLists() = {
      val nRuns = 2
      val nElements = 100000
      val delay = 3
      for(i <- 1 to nRuns) {
         val stream = asyncStream[AsyncList[Future,Int]] { out =>
              for(j <- 1 to nElements) {
                await(FutureSleep(delay.milliseconds))
                out.emit(i)
              }
         }
         val fList = stream.takeListAll()
         val timeToWait = (delay * nElements * 2).milliseconds
         println(s"AsyncListStressTest.testTimedGenList: waiting  ${timeToWait.toSeconds} seconds")
         Await.result(fList,timeToWait)
      }
    }

