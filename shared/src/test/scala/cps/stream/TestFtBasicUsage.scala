package cps

import org.junit.{Test,Ignore}
import org.junit.Assert._

import scala.concurrent.*
import scala.concurrent.ExecutionContext.Implicits.global


import cps.*
import cps.stream.*
import cps.monads.{*, given}

import cps.util.FutureCompleter

class TestFbBasicUsage:

  val N = 10000

  @Test def simpleLoopAsyncListFt() =

     val stream = asyncStream[AsyncList[Future, Int]]{ out =>
         for(i <- 1 to N) 
            out.emit(i)
     }

     val read = async[Future]{
       var done = false
       var current = stream
       var i = 1
       while(!done) {
         await(current.next) match
           case Some((h,next)) => 
                current = next
                assert(h == i)
                i = i + 1
           case None =>
                done = true
       }
     }
     FutureCompleter(read)



  @Test def simpleLoopExceptionAsyncListFt() =
     val stream = asyncStream[AsyncList[Future,Int]] { out =>
        for(i <- 1 to 10) {
          if (i == 5) then
            throw new RuntimeException("bye")
          out.emit(i)
        }
     }
     val listSum = stream.fold(0)(_ + _)
     val res = listSum.failed.map(ex => assert(ex.getMessage()=="bye"))
     FutureCompleter(res)


  @Test def testFib() =
     val stream = asyncStream[AsyncList[Future,Int]] { out =>
       var n = 1
       var m = 1 
       for(i <- 1 to 10) {
         out.emit(n)
         val tmp = n
         n = m
         m = m + tmp
       }
     }

     import scala.collection.mutable.ArrayBuffer
     val readAll = stream.fold(ArrayBuffer[Int]())((s,e)=>s.addOne(e))

     val res = readAll.map{ l =>
        assert(l(0)==1) 
        assert(l(1)==1) 
        assert(l(2)==2) 
        assert(l(3)==3) 
        assert(l(4)==5) 
        assert(l(5)==8) 
        assert(l(6)==13) 
        assert(l(7)==21) 
     }
     FutureCompleter(res)
    

