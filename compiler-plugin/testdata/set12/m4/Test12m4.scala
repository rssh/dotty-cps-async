package cpstest

import scala.concurrent.*
import scala.concurrent.duration.*
import scala.concurrent.ExecutionContext.Implicits.global

import cps.*
import cps.monads.{ *, given }

object Test12m4 {

  @cps.plugin.annotation.makeCPS
  def fetchList[T](c: List[T], f: List[T] => String): String = f(c)

  def main(args: Array[String]): Unit = {
    val fr = async[Future] {
      fetchList[String](
        List("myurl1", "myurl2", "myurl3"),
        l => l.mkString("{", ",", "}")
      )
    }
    val r  = Await.result(fr, 1000.millis)
    println(r)
  }
}
