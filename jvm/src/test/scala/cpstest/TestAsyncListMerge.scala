package cpstest

import org.junit.{Test,Ignore}
import org.junit.Assert.*

import scala.concurrent.*
import scala.concurrent.duration.*
import scala.util.*
import scala.util.control.*

import cps.*
import scala.concurrent.ExecutionContext.Implicits.global
import cps.monads.{*,given}
import cps.stream.*

import java.util.concurrent.CompletableFuture

import cps.util.*

class TestAsyncListMerge:

 
  @Test def testMergeTwoNonEmptyTimedLists() = {

    var last1: Int = -1
    var last2: Int = -1

    var cummulativeSleep1 = 0L
    var cummulativeRealSleep1 = 0L
    var lastRealSleep1 = 0L
    var cummulativeSleep2 = 0L
    var cummulativeRealSleep2 = 0L
    var lastRealSleep2 = 0L
    

    val stream1 = asyncStream[AsyncList[Future,Int]] { out =>
      for(i <- 1 to 10) {
        val sleepStart = System.currentTimeMillis()
        await(FutureSleep(100.milliseconds))
        val sleepEnd = System.currentTimeMillis()
        cummulativeSleep1 += 100
        lastRealSleep1 = sleepEnd - sleepStart
        cummulativeRealSleep1 += lastRealSleep1
        out.emit(i) 
        last1 = i
      }
      out.emit(100)
      last1 = 100
    }

    val stream2 = asyncStream[AsyncList[Future,Int]] { out =>
     for(i <- 200 to 220) {
      val sleepStart = System.currentTimeMillis()
      await(FutureSleep(50.milliseconds))
      val sleepEnd = System.currentTimeMillis()
      val lastRealSleep2 = sleepEnd - sleepStart
      cummulativeRealSleep2 += lastRealSleep2
      cummulativeSleep2 += 50
      out.emit(i)
      last2 = i
     }
     out.emit(300)
     last2 = 300
    }

    val stream = stream1 merge stream2

    val f = stream.takeListAll().map{ l =>
      val set = l.toSet
      assert(set.contains(100))
      for(i <- 1 to 10) {
        assert(set.contains(i))
      }
      for(i <- 200 to 220) {
        assert(set.contains(i))
      }
      assert(set.contains(300))
    }

    try
      val r = Await.result(f, 10.second)
    catch
      case NonFatal(ex) =>
        println(s"last1=$last1, last2=$last2")
        println(s"cummulativeSleep1=$cummulativeSleep1, cummulativeRealSleep1=$cummulativeRealSleep1, lastRealSleep1=${lastRealSleep1}")
        println(s"cummulativeSleep2=$cummulativeSleep2, cummulativeRealSleep2=$cummulativeRealSleep2, lastRealSleep2=${lastRealSleep2}")
        throw ex;

  }


  @Test def testMergeingTwoNonEmtpyNonTimedLists() = {

    val stream1 = asyncStream[AsyncList[Future,Int]] { out =>
      for(i <- 1 to 10) {
         out.emit(i) 
      }
      out.emit(100)
    }

    val stream2 = asyncStream[AsyncList[Future,Int]] { out =>
      for(i <- 200 to 220) {
        out.emit(i)
      }
      out.emit(300)
    }

    val stream = stream1 merge stream2

    val f = stream.takeListAll().map{ l =>
      val set = l.toSet
      assert(set.contains(100))
      for(i <- 1 to 10) {
        assert(set.contains(i))
      }
      for(i <- 200 to 220) {
        assert(set.contains(i))
      }
      assert(set.contains(300))
    }

    val r = Await.ready(f, 1.second)
 
  }

  
  @Test def testMergingEmptyList() = {

    val emptyStream = asyncStream[AsyncList[Future,Int]]{ out =>
      // do nothing
    }

    val stream2 = asyncStream[AsyncList[Future, Int]] { out =>
      for(i <- 1 to 10) out.emit(i)
    }

    val stream = emptyStream merge stream2

    val f = stream.takeListAll().map{ l =>
      assert(l == List(1,2,3,4,5,6,7,8,9,10))
    }

    val r = Await.ready(f, 1.second)

  }



