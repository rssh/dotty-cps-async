package cps.stream

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

  def asyncTest(p: Int=>Boolean, x:Int): Future[Boolean] =
    Future.successful(p(x))   

  @Test def testFilterAsync()=
    val r = async[Future] {

      val stream = asyncStream[AsyncList[Future, Int]]{ out =>
        for(i <- 1 to N) 
         out.emit(i)
      }

      val filtered = stream.filter(x => await(asyncTest(_ % 2 == 0, x)))

      val firstTen = await(filtered.takeList(10))

      assert(firstTen(0)==2)
      assert(firstTen(1)==4)
      assert(firstTen(2)==6)
      assert(firstTen(3)==8)

    }
    FutureCompleter(r)

    


