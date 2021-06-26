package cps.pe

import scala.language.implicitConversions

import org.junit.{Test,Ignore}
import org.junit.Assert._

import scala.quoted._
import scala.util.*

import cps.*
import cps.util.FutureCompleter
import scala.concurrent.ExecutionContext.Implicits.global


class TestPEPartialMemoizing:

  def qqq = 1

  def doTenTimes[X](effect: PureEffect[X]):PureEffect[Unit] = async[PureEffect] {
    for(i <- 1 to 10) {
      val tmp = await(effect) 
    }
  }
    
  def doSomething(x:Int, logger: PEToyLogger) = 
     logger.log(s"received $x")
     
  def effect(ref: PEIntRef, logger: PEToyLogger) = async[PureEffect]{
     val r = await(ref.increment())
     await(logger.log(s"ref:$r"))
     r
  }
   
  @Test def testPartial() = 
     import cps.automaticColoring.{*,given}
     var ref1 = PEIntRef.make(0)
     val logger1 = new PEToyLogger()
     val c1 = async[PureEffect] {
         val v = effect(ref1, logger1)
         doSomething(v, logger1)
     }
     var ref2 = PEIntRef.make(0)
     val logger2 = new PEToyLogger()
     val c2 = async[PureEffect] {
         await(doTenTimes(effect(ref2,logger2)))
     }
     val c = c1.flatMap(_ =>c2)
     val future = c.unsafeRunFuture().map{ _ =>
         println(s"ref1 = ${ref1}")
         println(s"log1 = ${logger1.__all()}")
         println(s"ref2 = ${ref2}")
         println(s"log2 = ${logger2.__all()}")
     }
     FutureCompleter(future)


  @Test def testPartialMixed() = 
     import cps.automaticColoring.{*,given}
     var ref1 = PEIntRef.make(0)
     val logger1 = new PEToyLogger()
     val code = 
     """
      async[PureEffect] {
         val v = effect(ref1, logger1)
         doSomething(v, logger1)
         doTenTimes(v)
      }
     """
     val errors = compiletime.testing.typeCheckErrors(code)
     println(s"errors: $errors")
     assert(!compiletime.testing.typeChecks(code))





