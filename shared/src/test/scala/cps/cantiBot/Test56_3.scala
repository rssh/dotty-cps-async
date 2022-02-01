package cps.cantiBot

import org.junit.{Test,Ignore}
import org.junit.Assert.*

import cps.*
import cps.monads.FutureAsyncMonad
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class Test56_3:

  def thing(): Future[String] =
      async[Future] {
        for _ <- Nil do
          ???

        await({
          Option("") match
            case None =>
              ??? : Future[String]
            case Some(_) =>
              ??? : Future[String]
        })
      }

  @Test def testThing() = 
      val f = thing()




