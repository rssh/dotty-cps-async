package cps

import org.junit.{Test,Ignore}
import org.junit.Assert._

import scala.concurrent._
import scala.concurrent.duration._
import scala.language.postfixOps
import scala.language.implicitConversions
import scala.quoted._
import scala.util.Success

import cps.customValueDiscard.given 
import cps.monads.given

class TestCustomValueDiscard:

  import scala.concurrent.ExecutionContext.Implicits.global 

  case class MyObj(value:Int)
  

  @Test def withCustomValueDiscard(): Unit = 
     //implicit val printCode = cps.macroFlags.PrintCode
     //implicit val printTree = cps.macroFlags.PrintTree
     //implicit val debugLevel = cps.macroFlags.DebugLevel(10)
     
     var x = 0
     given ValueDiscard[MyObj] with
        override def apply(value:MyObj) = 
           x += value.value

     val c = async[ComputationBound]{ 
         await(T1.cbt(MyObj(1)))
         await(T1.cbt(MyObj(2)))
     }
     val r = c.run()
     assert(r.isSuccess)
     assert(x == 1)

     x = 0
     val c1 = async[ComputationBound]{ 
         await(T1.cbt(MyObj(1)))
         await(T1.cbt(MyObj(2)))
         ()
     }
     val r1 = c1.run()
     assert(r1.isSuccess)
     assert(x == 3)

  class API(dryResult: Boolean) {
     def fetch(url:String): Future[String] = Future.successful(url)
     def dryRun(data:String): Future[Unit] =
           if (dryResult)
             Future successful ()
           else
             Future failed (new RuntimeException("be-be-be"))
     def process(data:String): Future[String] = Future successful data
  }

  @Test def apiInsideSeq(): Unit = 
     import cps.warnValueDiscard.given
     val api = API(false)
     val dryRunEnabled = true
     //implicit val printCode = cps.macroFlags.PrintCode
     //implicit val debugLevel = cps.macroFlags.DebugLevel(10)
     val r = async[Future] {
       val data = await(api.fetch("http://www.example.com"))
       //if (dryRunEnabled)
       api.dryRun(data)
       await(api.process(data))
     }

