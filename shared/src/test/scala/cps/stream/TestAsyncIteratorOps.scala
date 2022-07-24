package cps.stream

import org.junit.{Test,Ignore}
import org.junit.Assert._

import scala.concurrent.*
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.*
import java.util.concurrent.atomic.*

import cps.*
import cps.stream.*
import cps.monads.{*, given}

import cps.testconfig.given
import cps.util.FutureCompleter

class TestAsyncIteratorOps {


    
    @Test  def testSimpleMap() = {
        val stream = AsyncList.iterate[Future,Int](1 to 10)
        val iterator = stream.iterator
        val mappedIterator = iterator.map(x => x.toString)
        val f = async[Future] {
          var  i = 1
          while{
            await(mappedIterator.next) match
              case Some(x) => 
                  assert(i.toString == x)
                  i = i + 1
                  true
              case None =>   
                  assert(i == 11)
                  false
          } do ()
        } 
        FutureCompleter(f)
    }

    @Test  def testSimpleMapAsync() = {
      val stream = AsyncList.iterate[Future,Int](1 to 10)
      val iterator = stream.iterator
      val f = async[Future] {
        val mappedIterator = iterator.map(x => await(Future successful x.toString))
        var  i = 1
        while{
          await(mappedIterator.next) match
            case Some(x) => 
                assert(i.toString == x)
                i = i + 1
                true
            case None =>   
                assert(i == 11)
                false
        } do ()
      } 
      FutureCompleter(f)
    }

    @Test def testSimpleFilter() = {
      val stream = AsyncList.iterate[Future,Int](1 to 10)
      val iterator = stream.iterator
      val filtered = iterator.filter(_ % 2 == 0)
      val f = async[Future] {
        var i = 0
        while{
          val optValue = await(filtered.next)
          optValue.foreach{ x =>
            assert(x % 2 == 0)
            i = i + 1
          }
          optValue.isDefined
        } do ()
        assert(i == 5)
      }
      FutureCompleter(f)
    }

    @Test def testSimpleFilterAsync() = {
      val stream = AsyncList.iterate[Future,Int](1 to 10)
      val iterator = stream.iterator
      val f = async[Future] {
        val filtered = iterator.filter(_ % 2 == await(Future successful 0) )
        var i = 0
        while{
          val optValue = await(filtered.next)
          optValue.foreach{ x =>
            assert(x % 2 == 0)
            i = i + 1
          }
          optValue.isDefined
        } do ()
        assert(i == 5)
      }
      FutureCompleter(f)
    }

    @Test def testFindPos() = {
      val stream = AsyncList.iterate[Future,Int](1 to 10)
      val iterator = stream.iterator
      val f = async[Future] {
        val x = await(iterator.find(_ > 8))
        assert(x == Some(9))
      }
      FutureCompleter(f)
    }

    @Test def testFindNeg() = {
      val stream = AsyncList.iterate[Future,Int](1 to 10)
      val iterator = stream.iterator
      val f = async[Future] {
        val x = await(iterator.find(_ > 800))
        assert(x == None)
      }
      FutureCompleter(f)
    }

    @Test def testFindAsycPos() = {
      val stream = AsyncList.iterate[Future,Int](1 to 10)
      val iterator = stream.iterator
      //implicit val printCode = cps.macros.flags.PrintCode
      //implicit val debugLevel = cps.macros.flags.DebugLevel(20)
      val f = async[Future] {
        val x = await(iterator.find(_ > await(Future successful 8)))
        assert(x == Some(9))
      }
      FutureCompleter(f)
    }

    @Test def testFold() = {
      val stream = AsyncList.iterate[Future,Int](1 to 3)
      val iterator = stream.iterator
      val f = async[Future]{
           val fr = iterator.fold(0)(
                    (x:Int,y:Int) => x + y + 1
           )
           val r = await(fr)
           assert(r == 9)
      }
      FutureCompleter(f)
    }


    @Test def testFoldAsync(): Unit = {
      val stream = AsyncList.iterate[Future,Int](1 to 3)
      val iterator = stream.iterator
      val f = async[Future]{
           val fr = iterator.fold(0)(
                    (x:Int,y:Int) => x + y + await(Future.successful(1)) 
           )
           val r = await(fr)
           assert(r == 9)
      }
      FutureCompleter(f)
    }

    @Test def testScan() = {
      val stream = AsyncList.iterate[Future,Int](1 to 5)
      val iterator = stream.iterator
      val ft = async[Future]{
           val fr = iterator.scan(0)( (x,y) => x + y )
           val r0 = await(fr.next)
           assert(r0.get == 0)
           val r1 = await(fr.next)
           assert(r1.get == 1)
           val r2 = await(fr.next)
           assert(r2.get == 3)
           val r3 = await(fr.next)
           assert(r3.get == 6)
           val r4 = await(fr.next)
           assert(r4.get == 10)
           val r5 = await(fr.next)
           assert(r5.get == 15)
           val r6 = await(fr.next)
           assert(r6 == None)
      }
      FutureCompleter(ft)
    }

    
    @Test def testScanAsync() = {
      val stream = AsyncList.iterate[Future,Int](1 to 5)
      val iterator = stream.iterator
      val ft = async[Future]{
           val fr = iterator.scan(0)( (x,y) => x + await(Future.successful(y)) )
           val r0 = await(fr.next)
           assert(r0.get == 0)
           val r1 = await(fr.next)
           assert(r1.get == 1)
           val r2 = await(fr.next)
           assert(r2.get == 3)
           val r3 = await(fr.next)
           assert(r3.get == 6)
           val r4 = await(fr.next)
           assert(r4.get == 10)
           val r5 = await(fr.next)
           assert(r5.get == 15)
           val r6 = await(fr.next)
           assert(r6 == None)
      }
      FutureCompleter(ft)
    }

    @Test def testInTry() = {
      val myIterator = new AsyncIterator[Future,Int] {
        val v = new AtomicInteger(0)
        override def next: Future[Option[Int]] = {
          val n = v.incrementAndGet() 
          if ((n % 2) == 0) {
              Future.failed(new RuntimeException("even"))
          } else if (n <= 100) {
              Future successful Some(n)
          } else Future successful None
        }
      }

      val tryIterator = myIterator.inTry
      val ft = async[Future] {
         val v1 = await(tryIterator.next)
         assert(v1.get == Success(1))
         val v2 = await(tryIterator.next)
         assert(v2.get.isFailure)
         val v3 = await(tryIterator.next)
         assert(v3.get == Success(3))
         val v4 = await(tryIterator.next)
         assert(v4.get.isFailure)
         val v5 = await(tryIterator.next)
         assert(v5.get == Success(5))
      }
      FutureCompleter(ft)
    }

    @Test
    def testMapTry() = {

      val myIterator = new AsyncIterator[Future,Int] {
        val v = new AtomicInteger(0)
        override def next: Future[Option[Int]] = {
          val n = v.incrementAndGet() 
          if ((n % 2) == 0) {
              Future.failed(new RuntimeException("even"))
          } else if (n <= 5) {
              Future successful Some(n)
          } else Future successful None
        }
      }

      val mappedIterator = myIterator.mapTry{
        case Success(x) => x
        case Failure(ex) => 1
      }

      val ft = async[Future] {
         val v1 = await(mappedIterator.next)
         assert(v1.get == 1)
         val v2 = await(mappedIterator.next)
         assert(v2.get == 1)
         val v3 = await(mappedIterator.next)
         assert(v3.get == 3)
         val v4 = await(mappedIterator.next)
         assert(v4.get == 1)
         val v5 = await(mappedIterator.next)
         assert(v5.get == 5)
         val v6 = await(mappedIterator.next)
         assert(v6.get == 1)
         val v7 = await(mappedIterator.next)
         assert(v7 == None)
      }
      FutureCompleter(ft)
    }

    @Test
    def testMapTryAsync() = {
      val myIterator = new AsyncIterator[Future,Int] {
        val v = new AtomicInteger(0)
        override def next: Future[Option[Int]] = {
          val n = v.incrementAndGet() 
          if ((n % 2) == 0) {
              Future.failed(new RuntimeException("even"))
          } else if (n <= 5) {
              Future successful Some(n)
          } else Future successful None
        }
      }

      val delayedOne = Future successful 1

      val ft = async[Future] {

        val mappedIterator = myIterator.mapTry{
          case Success(x) => x + await(delayedOne)
          case Failure(ex) => -1
        }

        val v1 = await(mappedIterator.next)
        assert(v1.get == 2)
        val v2 = await(mappedIterator.next)
        assert(v2.get == -1)
      }
      FutureCompleter(ft)
    }

    @Test
    def testTake1() = {
      val stream1 = AsyncList.iterate[Future,Int](1 to 3)
      val iterator = stream1.iterator
      val ft = async[Future] {
        val l1 = await(iterator.take[List](2))
        assert(l1 == List(1,2))
        val stream2 = AsyncList.iterate[Future,Int](1 to 3)
        val it2 = stream2.iterator
        val l2 = await(it2.take[List](10))
        assert(l2 == List(1,2,3))
      }
      FutureCompleter(ft)
    }
    
    @Test
    def testTakeManyReaders() = {
      val stream1 = AsyncList.iterate[Future,Int](1 to 3)
      val iterator = stream1.iterator
      val ft = async[Future] {
        val l1 = await(iterator.take[List](2))
        assert(l1 == List(1,2))
        println(s"stream1 = $stream1")
        val it2 = stream1.iterator
        val l2 = await(it2.take[List](10))
        assert(l2 == List(1,2,3))
      }
      FutureCompleter(ft)
    }


}