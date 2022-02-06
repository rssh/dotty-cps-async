package cpstest

import org.junit.{Test,Ignore}
import org.junit.Assert.*

import scala.concurrent.*
import scala.concurrent.duration.*
import scala.util.*

import cps.*
import scala.concurrent.ExecutionContext.Implicits.global
import cps.monads.{*,given}

import java.util.concurrent.CompletableFuture

class TestInferredByDefault {

  //given CpsAsyncMonad[Future] = FutureAsyncMonad
  
  import cps.monads.FreeCpsMonad

  @Test def testInferredParam() = {
     val v = async{
       val s = summon[CpsMonadContext[?]].toString
       println(s)
       s
     }
     v match {
       case cf: CompletableFuture[?] =>
          println("CompletableFuture")
       case f: scala.concurrent.Future[?] =>
          println("Future")
       case _ =>
          println(s"unknown type $v")
     }

  }


}