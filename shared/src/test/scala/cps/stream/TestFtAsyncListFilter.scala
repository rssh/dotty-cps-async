package cps

import org.junit.{Test,Ignore}
import org.junit.Assert._

import scala.concurrent.*
import scala.concurrent.ExecutionContext.Implicits.global


import cps.*
import cps.stream.*
import cps.monads.{*, given}

import cps.util.FutureCompleter

class TestFbBAsyncListFilter:

  val N = 10000

  @Test def filterOdd() =

     val stream = asyncStream[AsyncList[Future, Int]]{ out =>
         for(i <- 1 to N) 
            out.emit(i)
     }

     val filtered = stream.filter(_ % 2 == 0)

     val read = async[Future]{
       var done = false
       var current = filtered
       var i = 1
       while(!done) {
         await(current.next) match
           case Some((h,next)) => 
                current = next
                assert(h == i*2)
                i = i + 1
           case None =>
                done = true
       }
     }
     FutureCompleter(read)




