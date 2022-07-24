package cpstest

import org.junit.{Test,Ignore}
import org.junit.Assert.*

import scala.concurrent.*
import scala.concurrent.duration.*
import scala.util.*
import scala.util.control.*

import cps.*
import scala.concurrent.ExecutionContext.Implicits.global
import cps.monads.{*,given}
import cps.testconfig.given

import java.util.concurrent.CompletableFuture


class TestCompletableFutureTryCatch:

  given CpsSchedulingMonad[CompletableFuture] = CompletableFutureCpsMonad

  def raise(x: Throwable): Unit = throw x

  @Test def testSimpleTryCatch0() = {

    var x = 0

    val a1 = async { 
               try
                 raise(new RuntimeException("CompletableFuture:simpleTryCatch:0")) 
               catch
                 case NonFatal(ex) =>
                   x = 1
    }

    val f1 = toFutureConversion[CompletableFuture, Unit](a1)
    Await.ready(f1, 1.second)

    assert(x == 1)

  }

  @Test def testSimpleTryCatch1() = {

    var x = 0
    val a = async { 
               try
                 raise(new RuntimeException("CompletableFuture:simpleTryCatch:1.1")) 
               catch
                 case NonFatal(ex) =>
                   x = 1
                   raise(new RuntimeException("CompletableFuture:simpleTryCatch:1.2")) 
    }
    val f = toFutureConversion[CompletableFuture, Unit](a)
    Await.ready(f, 1.second)
    assert(x == 1)
    val r = f.value.get
    assert(r.isFailure)
    val Failure(ex) = r
    assert(ex.getMessage().nn.contains("CompletableFuture:simpleTryCatch:1.2"))

  }


  @Test def testSimpleTryFinally2() = {

    var x = 0
    val a = async { 
               try
                  1
               finally
                 x = 1
                 raise(new RuntimeException("CompletableFuture:simpleTryFinally:2")) 
    }
    val f = toFutureConversion[CompletableFuture, Unit](a)
    Await.ready(f, 1.second)
    assert(x == 1)
    val r = f.value.get
    assert(r.isFailure)
    val Failure(ex) = r
    assert(ex.getMessage().nn.contains("CompletableFuture:simpleTryFinally:2"))

  }


  @Test def testSimpleTryFinally3() = {

    //implicit val printCode = cps.macros.flags.PrintCode

    var x = 0
    var y = 0
    val a = async { 
               try
                  raise(new RuntimeException("CompletableFuture:simpleTryFinally:3.1")) 
                  1
               finally
                 x = 1
                 raise(new RuntimeException("CompletableFuture:simpleTryFinally:3.2")) 
                 y = 2
    }
    val f = toFutureConversion[CompletableFuture, Unit](a)
    Await.ready(f, 1.second)
    assert(x == 1)
    assert(y == 0)
    val r = f.value.get
    assert(r.isFailure)
    val Failure(ex) = r
    //println(ex.getMessage())
    // unchanged: looks like scala and java behavious here is differ.
    //assert(ex.getMessage().contains("CompletableFuture:simpleTryFinally:3.1"))

  }

  @Test def testAsyncTryCatch4() = {

    var x = 0
    val a = async { 
               try
                 await(CompletableFuture.completedFuture(1).nn)
                 raise(new RuntimeException("CompletableFuture:simpleTryCatch:4.1")) 
               catch
                 case NonFatal(ex) =>
                   x = 1
                   raise(new RuntimeException("CompletableFuture:simpleTryCatch:4.2")) 
    }
    val f = toFutureConversion[CompletableFuture, Unit](a)
    Await.ready(f, 1.second)
    assert(x == 1)
    val r = f.value.get
    assert(r.isFailure)
    val Failure(ex) = r
    assert(ex.getMessage().nn.contains("CompletableFuture:simpleTryCatch:4.2"))

  }

