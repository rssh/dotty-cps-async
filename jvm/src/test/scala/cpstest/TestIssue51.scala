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


class TestIssue51:


  def raise(x: Throwable): Unit = throw x

  @Test def asyncShouldReportErrors() = {

    given CpsSchedulingMonad[Future] = FutureAsyncMonad

    val ex1 = IllegalArgumentException("blah")
    val ex2 = IllegalArgumentException("blah 2")
    val ex3 = IllegalArgumentException("blah 3")
    val a1 = async { raise(ex1) }
    val a2 = async { await(a1) }
    val a3 = async { raise(ex2); await(a1) }
    val a4 = async { 5 }
    val a5 = async { await(a4); raise(ex3) }
    val a6 = async { await(a4); await(a4); await(a5); await(a1) }
    val a7 = async { await(a4); await(a4); await(a1); await(a5) }

    Await.ready(a1, 1.second)
    Await.ready(a2, 1.second)
    Await.ready(a3, 1.second)
    Await.ready(a4, 1.second)
    Await.ready(a5, 1.second)
    Await.ready(a6, 1.second)
    Await.ready(a7, 1.second)
 
    assert( a1.value == Some(Failure(ex1)) )
    assert( a2.value == Some(Failure(ex1)) )
    assert( a3.value == Some(Failure(ex2)) )
    assert( a4.value == Some(Success(5))   )
    assert( a5.value == Some(Failure(ex3)) )
    assert( a6.value == Some(Failure(ex3)) )
    assert( a7.value == Some(Failure(ex1)) )

  }


  @Test def asyncShouldReportErrorsCompletableFuture() = {

    given CpsSchedulingMonad[CompletableFuture] = CompletableFutureCpsMonad

    val a1 = async { raise(new RuntimeException("CompletableFuture:a1")) }
    val f1 = toFutureConversion[CompletableFuture, Unit](a1)
    Await.ready(f1, 1.second)
    assert( f1.value.get.isFailure )
    val Some(Failure(ex1)) = f1.value

  }

