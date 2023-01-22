package cpstest

import org.junit.{Test,Ignore}
import org.junit.Assert._

import scala.concurrent.*
import scala.util.*

import cps.*
import cps.monads.{*,given}
import cps.testconfig.given
import cps.util.FutureCompleter


import cps.syntax.monadless.*
import scala.concurrent.ExecutionContext.Implicits.global


class TestMonadlessSyntax {


  @Test 
  def testMonadlessSyntsxOnComputationBound() = {
     val c = lift[ComputationBound] {
         val x = unlift(T1.cbi(1)) + unlift(T1.cbi(2))
         x
     }
     val r = c.run()
     assert(r.isSuccess)
     r match
       case Success(v) => assert(v == 3)
       case Failure(ex) => throw ex
  }

  @Test
  def testInferenceForFutureMonad() = {

    import cps.monads.FutureAsyncMonad 

    // minimal API
    def badRequest: Future[String] = Future failed RuntimeException("bad request")
    def responseToString(f:String): String = f

    val responseStringC: Future[String] = lift {
      try {
        responseToString(unlift(badRequest))
      } catch {
        case e: Exception => s"received an exceptional result: $e"
      }
    }

    FutureCompleter(responseStringC)
  }



}

