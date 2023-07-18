package cps.cantiBot

import cps.*
import cps.monads.FutureAsyncMonad

import cps.plugin.annotation.*
import cps.macros.flags.UseCompilerPlugin
given UseCompilerPlugin.type = UseCompilerPlugin

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

@CpsDebugLevel(20)
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

