package cps.cantiBot

import org.junit.{Test,Ignore}
import org.junit.Assert.*

import cps.*
import cps.monads.FutureAsyncMonad
import cps.testconfig.given

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class Test56_2:

  def thing(): Future[String] =
      async[Future] {
        for _ <- Nil do
          ???

        Option("") match
          case None =>
            await(??? : Future[String])
          case Some(_) =>
            await(??? : Future[String])
      }

  @Test def testThing() = 
      val f = thing()




