package cpstest

import org.junit.{Test,Ignore}
import org.junit.Assert.*

import scala.concurrent.*
import scala.concurrent.duration.*
import scala.concurrent.ExecutionContext.Implicits.global

import cps.*
import cps.monads.{*,given}


class TestMinimalExample:

  val output = false

  def fetchGreeting(): Future[String] =
    Future successful "Hi"

  def greet() = async[Future] {
    val greeting = await(fetchGreeting())
    if output then
       println(greeting)
  }

  @Test def testMinimalGreeting(): Unit = {
     Await.ready(greet(), 1.second)
  }



