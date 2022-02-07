package cps.util

import scala.concurrent._
import scala.concurrent.duration._
import scala.util._

import scala.scalanative.loop._

// for use futures in jvm and native test.
object FutureCompleter
{

  def apply[T](f: Future[T])(using ec:ExecutionContext): Unit = 
    EventLoop.run()
    Await.result(f, 30.seconds)

}
