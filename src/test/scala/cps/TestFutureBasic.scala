package cps

import org.junit.{Test,Ignore}
import org.junit.Assert._

import scala.concurrent._
import scala.concurrent.duration._
import scala.language.postfixOps
import scala.quoted._
import scala.util.Success


class TestFutureBasic:

  import scala.concurrent.ExecutionContext.Implicits.global 

  @Test def futureBasic1(): Unit = 
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
     println(s"r=$r")
     assert(r==(5050+100))





