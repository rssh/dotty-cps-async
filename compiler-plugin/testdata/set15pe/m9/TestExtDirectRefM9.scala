package cps.pe

import org.junit.{Test,Ignore}
import org.junit.Assert.*

import scala.util.*


import scala.annotation.experimental
import scala.concurrent.*
import scala.concurrent.duration.*

import cps.*
import cps.plugin.annotation.CpsDebugLevel
import scala.concurrent.ExecutionContext.Implicits.global

@experimental
@CpsDebugLevel(20)
class TestExtDirectRefM9:

  import cps.pe.directRef.*


  def runExtDirectRef(): PureEffect[String] = {
    implicit val printCode = cps.macros.flags.PrintCode
    implicit val debugLevel = cps.macros.flags.DebugLevel(20)
    async[PureEffect] {
      //val logger = PEToyLogger.make()
      val vs = PureEffect.directRef("AAAa")
      vs.set("0")
      //logger.log(vs.get)
      vs.get
    }
  }

  @Test def testExtFirectRefM9(): Unit =
    val c = runExtDirectRef()
    println(s"PE:extDirectRef, c=${c} ")
    val future = c.unsafeRunFuture().map{ vs =>
      println(s"PE:extDirectRef, vs=${vs} ")
      assertTrue(vs=="0")
    }
    val r = Await.result(future,1.second)

