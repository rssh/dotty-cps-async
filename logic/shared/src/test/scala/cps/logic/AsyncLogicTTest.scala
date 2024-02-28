package cps.logic

import scala.concurrent.*
import scala.concurrent.duration.*
import scala.concurrent.ExecutionContext.Implicits.global
import cps.*
import cps.monads.FutureAsyncMonad
import cps.monads.logic.{*, given}
import org.junit.Test




class AsyncLogicTTest {

}

object AsyncLogicTTest {



  object NetworkAPI {

    enum FetchBehaviour {
      case Success
      case Fail
      case Timeout
    }

    def fetch(url:String, behaviour: FetchBehaviour):Future[String] = {
      behaviour match
        case FetchBehaviour.Success => Future.successful(s"success: $url")
        case FetchBehaviour.Fail => Future.failed(new RuntimeException(s"Fail $url"))
        case FetchBehaviour.Timeout => Future.never
    }

  }

  def asyncAwaitInLogicT(urls:List[String]): LogicStreamT[Future, String] = reify[[X] =>> LogicStreamT[Future, X]] {
    val url = choices.from(urls)
    val result = await(NetworkAPI.fetch(url, NetworkAPI.FetchBehaviour.Success))
    result
  }

  @Test def testAsyncAwaitInLogicT(): Unit = {
    val urls = List("http://example.com", "http://example.org")
    val logicStream = asyncAwaitInLogicT(urls)
    val firstResult = logicStream.observeOne
    val r = Await.result(firstResult, 1.second)
    assert(r.size == 2)
    assert(r.contains("success: http://example.com"))
    assert(r.contains("success: http://example.org"))
  }

}