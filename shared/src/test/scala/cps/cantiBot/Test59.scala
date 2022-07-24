package cps.cantiBot

import cps.*
import cps.monads.FutureAsyncMonad
import cps.testconfig.given

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class TestCase:
  val objs = Seq(
    new AnyRef:
      def curry(v: Any)(w: Any): Unit = ()

      def bad_func(): Future[Unit] =
        async[Future] {
          for message <- Nil.toSeq do
            curry(Nil.toSeq)
          ()
        }
  )

