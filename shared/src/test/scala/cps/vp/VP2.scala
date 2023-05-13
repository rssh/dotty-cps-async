package cps.vp

import cps.*
import cps.monads.{*,given}

import scala.concurrent.*
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.control.NonFatal

object VP2:

  case class TestContext(clientId: String, url: String, trace: Boolean = false)


  class Test {
    def run(ctx:TestContext): Future[Unit] = ???
  }

  val ExtremeSize = new Test
  def tests: Seq[Test] = ???

  def run1(url: String, clientId: String): Future[Unit] = async[Future] {  
    val testContext = TestContext(clientId, url)
    for(test <- tests) {
      println(s"running ${test}")
      try {
        await(test.run(testContext))
      }catch{
        case NonFatal(ex) =>
          if !(test eq ExtremeSize) then
            println(s"exception during test: ${ex.getMessage} rerunning with trace")
            //bug in dotty: https://github.com/lampepfl/dotty/issues/17445
            //val traceContext = testContext.copy(trace=true)
            val traceContext = TestContext(testContext.clientId, testContext.url, true)
            try {
              await(test.run(traceContext))
            } catch {
              case NonFatal(ex) =>
            }
          else
            println(s"exception during test ${test}, ${ex.getMessage}")
      }
    }
    println(s"summary metrics: ${testContext}")
  }


