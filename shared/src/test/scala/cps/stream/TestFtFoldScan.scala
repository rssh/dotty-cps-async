package cps

import org.junit.{Test,Ignore}
import org.junit.Assert._

import scala.concurrent.*
import scala.concurrent.ExecutionContext.Implicits.global


import cps.*
import cps.stream.*
import cps.monads.{*, given}

import cps.util.FutureCompleter

class TestFbFoldScan:

  @Test def testFold() =
    val stream = asyncStream[AsyncList[Future, Int]]{ out =>
      for(i <- 1 to 100) 
        out.emit(i)
    }
    val sumFuture = stream.fold(0)((s,e)=>s+e)
    val read = async[Future]{
      val sum = await(sumFuture)
      val sum1 = (1 to 100).sum
      assert(sum == sum1)
    }
    FutureCompleter(read)

  @Test def testFoldAsync() =
    val check = async[Future] {
      val stream = asyncStream[AsyncList[Future, Int]]{ out =>
        for(i <- 1 to 10) 
          out.emit(i)
      }
      val sumFuture = stream.fold(0)((s,e)=>s+await(Future successful e))
      val sum = await(sumFuture)
      val sum1 = (1 to 10).sum
      assert(sum == sum1)
    }
    FutureCompleter(check)
  

  @Test def testScan() =
    val stream = asyncStream[AsyncList[Future, Int]]{ out =>
      for(i <- 1 to 10) 
        out.emit(i)
    } 
    val scan = stream.scan(0)((s,e) => s + e)
    val fScanList = scan.takeListAll()
    val check = async[Future] {
        val scanList = await(fScanList)
        val sampleScan = (1 to 10).scanLeft(0)((s,e)=>s+e)
        assert(scanList == sampleScan)
    }
    FutureCompleter(check)

  @Test def testScanAsync() =
    val stream = asyncStream[AsyncList[Future, Int]]{ out =>
      for(i <- 1 to 10) 
        out.emit(i)
    } 
    val check = async[Future] {
      val scan = stream.scan(0)((s,e) => s + await(Future successful e))
      val scanList = await(scan.takeListAll())
      val sampleScan = (1 to 10).scanLeft(0)((s,e)=>s+e)
      assert(scanList == sampleScan)
    }
    FutureCompleter(check)

