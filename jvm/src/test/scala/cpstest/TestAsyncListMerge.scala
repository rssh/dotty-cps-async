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


class TestAsyncListMerge:

  given CpsSchedulingMonad[CompletableFuture] = CompletableFutureCpsMonad


  @Test def testMergeTwoNonEmptyTimedLists() = {

    val stream1 = asyncStream[AsyncList[Future,Int]] { out =>
      for(i <- 1 to 10) {
         Thread.sleep(100)
         out.emit(i) 
      }
      out.emit(100)
    }

    val stream2 = asyncStream[AsyncList[Future,Int]] { out =>
     for(i <- 200 to 220) {
       Thread.sleep(50)
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

    val r = Await.result(f, 2.second)

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



