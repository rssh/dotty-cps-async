package cpstest

import scala.concurrent.*
import scala.concurrent.duration.*
import scala.concurrent.ExecutionContext.Implicits.global

import cps.*
import cps.monads.{ *, given }

object Test12m2 {

  @cps.plugin.annotation.makeCPS
  def fetchConst(c: String, f: String => String): String = f(c)

  def main(args: Array[String]): Unit = {
    val fr = async[Future] {
      fetchConst("myurl", name => "prefix" + name)
    }
    val r  = Await.result(fr, 1000.millis)
    println(r)
  }
}
