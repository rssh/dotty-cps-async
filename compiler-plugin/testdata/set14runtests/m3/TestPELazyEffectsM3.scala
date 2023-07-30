package cps.pe

import org.junit.{Test,Ignore}
import org.junit.Assert._

import scala.concurrent._
import scala.concurrent.duration._
import scala.util._

import cps._
import cps.testconfig.given

import scala.concurrent.ExecutionContext.Implicits.global

class TestPELazyEffectM3:


  @Test def testLazyEffectPrint(): Unit =
     System.out.println("lazyEffectPrint:1:out")
     System.err.println("lazyEffectPrint:1:err")
     val logger = new ToyLogger()
     System.out.println("lazyEffectPrint:2:out")
     System.err.println("lazyEffectPrint:2:err")
     val c = async[PureEffect]{ logger.log("W") }
     System.out.println(s"lazyEffectPrint:3:out, logger.lines = ${logger.lines}")
     assert(logger.lines.isEmpty)
     System.out.println("lazyEffectPrint:4:out")
     val future = c.unsafeRunFuture().map(x => assert(logger.lines(0)=="W") )
     println("before await")
     System.err.println("lazyEffectPrint:before await")
     //FutureCompleter(future)
     Await.ready(future, 1.second)
     System.err.println("lazyEffectPrint:after await")



