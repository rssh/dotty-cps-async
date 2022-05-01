package cps.stream

import org.junit.{Test,Ignore}
import org.junit.Assert._

import scala.concurrent.*
import scala.concurrent.ExecutionContext.Implicits.global


import cps.*
import cps.stream.*
import cps.monads.{*, given}

import cps.util.FutureCompleter

class TestAsyncListOps:



  @Test def simpleMapTake() =

     val stream = asyncStream[AsyncList[Future, Int]]{ out =>
         for(i <- 1 to 10) 
            out.emit(i)
     }

     val mapped = stream.map(_ + 1)

     val ft = async[Future]{
       val l = await(mapped.take[List](5))
       assert(l == List(2,3,4,5,6))
     }
     FutureCompleter(ft)

  def asyncSuccess[A](a:A):Future[A] =
    Future successful a
    
  def asyncFailure(e:Throwable): Future[Nothing] =
    Future failed e

  def repeat[A](a:A, n:Int):AsyncList[Future,A] = asyncStream[AsyncList[Future,A]] { out =>
    for(i <- 1 to n) out.emit(a)
  }  

  @Test def testMapAsync() =
    val stream = AsyncList.iterate[Future,Int](1 to 10)

    val ft = async[Future] {
      val mapped = stream.map{ x =>
         x + await(asyncSuccess(1))
      }
      val l = await(mapped.take[List](5))
      assert(l == List(2,3,4,5,6))
    }
    FutureCompleter(ft)

  @Test def testFlatMap() =
    val stream = AsyncList.iterate[Future,Int](1 to 3)
    val ft = async[Future] {
        val flatMapped = stream.flatMap(x => repeat(x,x))
        val l1 = await(flatMapped.take[List](5))
        assert(l1 == List(1,2,2,3,3))
        val l2 = await(flatMapped.take[List](10))
        assert(l2 == List(1,2,2,3,3,3))
    }
    FutureCompleter(ft)

  
