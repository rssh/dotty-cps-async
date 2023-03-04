package cpstest


import scala.concurrent.{Future}
import cps.monads.FutureAsyncMonad
import cps.monads.FutureAsyncMonadAPI
import cps.async
import scala.concurrent.ExecutionContext

def hello: Future[?] = {

  given ExecutionContext = ExecutionContext.global

  async {
    val a: Seq[Generic[?]] = ???
    a
      .foreach { to =>
        to.mthd()
      }
  }
}

trait Generic[+T] {
  def mthd(): Generic[T] = this
}