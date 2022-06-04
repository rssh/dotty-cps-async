package futureScope.examples

import java.io.IOException

import scala.concurrent.*
import scala.concurrent.duration.*
import scala.util.*

import cps.*
import cps.monads.{*,given}
import futureScope.*

import cps.*
import cps.monads.{*,given}
import cps.util.FutureCompleter

import org.junit.{Test,Ignore}
import org.junit.Assert.*

class TestTenUrls {

    enum FetchResult {
      case Success(data: String, delay: FiniteDuration)
      case Failure(msg: String)
      case InfiniteWait
    }

    import scala.concurrent.ExecutionContext.Implicits.global

    class NetworkApiMock(records:Map[String,FetchResult]) extends TenUrls.NetworkApi {

      //val nOpened

      override def fetch(url: String)(using ctx: FutureScopeContext): Future[String] = async[Future].in(Scope.child(ctx)) {
        records.get(url) match
          case Some(record) =>
            record match 
              case FetchResult.Success(data, delay) =>
               await(FutureScope.spawnDelay(delay))
               data
              case FetchResult.Failure(msg) =>
               throw new IOException(msg)
              case FetchResult.InfiniteWait =>
               val p = Promise[String]
               await(p.future)  
          case None =>
              throw new IOException(s"Mock URL not found $url")      
      }
   
   
    }


    @Test def testSimplePages() = {
      val urlsData = (for(i <- 1 to 100) yield {
          (i.toString, FetchResult.Success(i.toString,i.milliseconds))
      } ).toMap
      val mockApi = NetworkApiMock(urlsData)
      val urls = urlsData.keys.toList
      val f = async[Future].in(Scope) {
        val first10 = await(TenUrls.readFirstN(mockApi,urls,10))
        assert(first10.length == 10)
      }
      FutureCompleter(f)
    }

    @Test def testRandomBehavious() = {
      val random = new Random(1)
      val urlsData = (for(i <- 1 to 100) yield {
           val p = random.nextDouble()
           val fetchResult = {
             if (p < 0.555) then
                FetchResult.Success(i.toString, random.nextInt(100).milliseconds)
             else if (p < 0.8888) then
                FetchResult.Failure(s"p=$p")
             else
                FetchResult.InfiniteWait   
           }
           (i.toString, fetchResult)
      } ).toMap
      val mockApi = NetworkApiMock(urlsData)
      val urls = urlsData.keys.toList
      val f = async[Future].in(Scope) {
        val first10 = await(TenUrls.readFirstN(mockApi,urls,10))
        assert(first10.length == 10)
      }
      FutureCompleter(f)
    }

 

}