package cpsloomtest

import cps.*
import cps.monads.{*,given}

import scala.concurrent.*
import scala.concurrent.duration.*
import org.junit.{Test,Ignore}



class TestHOFunctionArgLoomTransform {

  import scala.concurrent.ExecutionContext.Implicits.global

  def fetch(url:String):Future[String] =
    Future successful s"fetched(${url})"

  @Test
  def testAwaitInMap() = {
    val f = async[Future] {
      val list = MyList.create("http://example1.com", "http://example2.com", "http://example3.org")
      val fetched = list.map{ url =>
        await(fetch(url))
      }
      assert(fetched.length == 3)
    }
    Await.result(f, 1.second)
  }


}
