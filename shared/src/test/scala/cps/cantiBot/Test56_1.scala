package cps.cantiBot

import org.junit.{Test,Ignore}
import org.junit.Assert.*

import cps.*
import cps.monads.FutureAsyncMonad
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class Test56_1:

  def thing(): Future[String] =
      async[Future] {
        for _ <- Nil do
          ???

        Option("") match
          case None =>
          case Some(_) =>
            await(??? : Future[Unit])

        "Test"
      }

  @Test def testThing() = 
      val f = thing()




