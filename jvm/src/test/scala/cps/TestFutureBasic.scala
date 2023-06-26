package cps

import cps.monads.{*,given}
import cps.testconfig.given
//import cps.monads.FutureAsyncMonad
import org.junit.{Test,Ignore}
import org.junit.Assert._

import scala.concurrent._
import scala.concurrent.duration._
import scala.language.postfixOps
import scala.quoted._
import scala.util.Success

import java.util.Timer
import java.util.TimerTask

class TestFutureBasic:

  import scala.concurrent.ExecutionContext.Implicits.global 


  @Test def futureBasic1(): Unit =
     //implicit val printCode = cps.macros.flags.PrintCode
     //implicit val debugLevel = cps.macros.flags.DebugLevel(20)
     val p = Promise[Int]()
     val c = async[Future]{ 
       await(p.future) + 1
     }
     p.success(3)
     val r = Await.result(c, 10 seconds)
     assert(r==4)


  @Test def futureBasic2(): Unit = 
     //implicit val printCode = cps.macroFlags.PrintCode
     //implicit val debugLevel = cps.macroFlags.DebugLevel(20)
     def fun(x:Int):Future[Int] = 
       Future successful (x+1)
     val c = async[Future]{ 
       @volatile var s = 0
       for( i <- 1 to 100 )
          s += await(fun(i))
       s
     }
     val r = Await.result(c, 10 seconds)
     assert(r==(5050+100))

  object FetchEmulator:
 
    val timer = new Timer("fetchEmulator", true)

    def retrieve(url: String, delay: Long):Future[String] = 
      val p = Promise[String]()
      timer.schedule(new TimerTask{
          def run():Unit =
             p.success("OK")
        },
        delay)
      p.future
    


  @Test def futureFetchList(): Unit = 
     val c = async[Future]{ 
        val l = List("ürl1","url2", "url3")
        l.map(FetchEmulator.retrieve(_,50)).map(await(_))
     }
     val r = Await.result(c, 10 seconds)
     assert(r==List("OK","OK","OK"))


