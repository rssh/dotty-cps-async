package cpstest

import scala.concurrent.*
import scala.concurrent.duration.*
import scala.concurrent.ExecutionContext.Implicits.global

import cps.*
import cps.monads.{ *, given }

object Test12m4 {

  @cps.plugin.annotation.makeCPS
  def fetchList[T](c: List[T], f: List[T] => String): String = f(c)

  def asyncAppendTransformed[T](l:List[T]): Future[String] =
    Future.successful(l.mkString("{", ",", "}")+"transformed" )

  def main(args: Array[String]): Unit = {
    val fr = async[Future] {
      fetchList[String](
        List("myurl1", "myurl2", "myurl3"),
        l => await(asyncAppendTransformed(l))
      )
    }
    val r  = Await.result(fr, 1000.millis)
    println(r)
  }
}
