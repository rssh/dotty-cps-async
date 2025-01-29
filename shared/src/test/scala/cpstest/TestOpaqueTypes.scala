package cpstest

import org.junit.Test

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import cps.*
import cps.monads.{*,given}
import cps.util.FutureCompleter

opaque type TestOpaqueTypes0 = TestOpaqueTypes0.MyType

object TestOpaqueTypes0 {

  opaque type MyType = Int

  def createMyType(x: Int): MyType = x

  def createTestOpaqueType(x: Int): TestOpaqueTypes0 = createMyType(x)


  extension (x: TestOpaqueTypes0)
    def doAsyncOperation: Future[MyType] = Future.successful(x)

}

/*
opaque type TestOpaqueTypes1[X] = TestOpaqueTypes1.MyType[X]


object TestOpaqueTypes1 {

  opaque type MyType[X] = X

  def createMyType[X](x: X): MyType[X] = x

  def createTestOpaqueType[X](x: X): TestOpaqueTypes1[X] = createMyType(x)


  extension [X](x: TestOpaqueTypes1[X])
    def doAsyncOperation: Future[X] = Future.successful(x)
*/

class TestOpaqueTypes {

  @Test
  def testAsyncOpOnOpaqueType0() = {
    val f = async[Future] {
      val x = TestOpaqueTypes0.createTestOpaqueType(1)
      val y = await(x.doAsyncOperation)
      y
    }
    FutureCompleter(f)
  }

}
