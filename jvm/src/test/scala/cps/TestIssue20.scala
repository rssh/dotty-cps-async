package cps

import org.junit.{Test,Ignore}
import org.junit.Assert._

import scala.concurrent._
import scala.concurrent.duration._
import scala.language.postfixOps
import scala.quoted._
import scala.util.Success

import java.util.Timer
import java.util.TimerTask

import cps.monads.given
import cps.testconfig.given

class TestIssue20:

  import scala.concurrent.ExecutionContext.Implicits.global 

  class EffectHolder:
    var wasInCatch: Boolean = false
    var wasInFinally: Boolean = false
 

  def runFutureFull(h: EffectHolder): Future[String] =
    //implicit val printCode = cps.macros.flags.PrintCode
    //implicit val debugLevel = cps.macros.flags.DebugLevel(20)
    async[Future]{
      try
        val s = await(Future("hello"))
        val i = await(Future(throw new Exception))
        s * i
      catch
        case _: Exception => 
           h.wasInCatch=true
           "caught"
      finally
        h.wasInFinally = true
        // println("done")
   }
    
    


  def runFutureNoCatch(h: EffectHolder): Future[String] =
    async[Future]{
      try
        val s = await(Future("hello"))
        val i = await(Future(throw new Exception))
        s * i
      finally
        h.wasInFinally = true
        //println("done")
    }



  @Test def tryCatchFinallyBase(): Unit = 
     val h = EffectHolder()
     val future = runFutureFull(h)
     val r = Await.ready(future, 10 seconds)
     assert(h.wasInCatch, "was in catch failed")
     assert(h.wasInFinally, "was in finally failed")

  @Test def tryNoCatchFinallyBase(): Unit = 
     val h = EffectHolder()
     val future = runFutureNoCatch(h)
     val r = Await.ready(future, 10 seconds)
     assert(h.wasInFinally, "was in finally failed")




