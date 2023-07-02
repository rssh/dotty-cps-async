package cpstest

import org.junit.{Test,Ignore}
import org.junit.Assert._

import java.util.Timer
import java.util.TimerTask
import scala.concurrent.*
import scala.concurrent.duration.*

import cps.*
import cps.monads.{*,given}

import cps.macros.flags.*
import cps.plugin.annotation.CpsDebugLevel

given UseCompilerPlugin.type = UseCompilerPlugin


//@CpsDebugLevel(20)
class TestFutureBasic:

  import scala.concurrent.ExecutionContext.Implicits.global

  given cps.macros.flags.UseCompilerPlugin = cps.macros.flags.UseCompilerPlugin


  object FetchEmulator {

    val timer = new Timer("fetchEmulator", true)

    def retrieve(url: String, delay: Long): Future[String] =
      ???

  }

  @Test def futureFetchList(): Unit = {
    val c = async[Future] {
      val l = List("ürl1", "url2", "url3")
      l.map(FetchEmulator.retrieve(_, 50)).map(await(_))
    }
    val r = Await.result(c, 10.seconds)
    assert(r == List("OK", "OK", "OK"))
  }



