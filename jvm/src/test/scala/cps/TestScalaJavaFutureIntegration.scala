package cps


import org.junit.{Test,Ignore}
import org.junit.Assert._

import scala.language.implicitConversions
import scala.language.postfixOps
import scala.concurrent._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util._
import java.util.concurrent.CompletableFuture

import cps.monads.given
import cps.testconfig.given

class TestScalaJavaFutureIntegration:

   def fetchSomething(): CompletableFuture[String] =
     val r = new CompletableFuture[String]()
     r.complete("AAA")
     r

   @Test def testCall() = {
     val c = async[Future] {
        val x = await(fetchSomething())
        s"received (${x})"
     }
     val r = Await.result(c, 1 second)
     assert(r == "received (AAA)")
   }


