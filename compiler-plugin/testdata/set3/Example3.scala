package cpstest

import cps.*
import cps.monads.{*,given}
import scala.annotation.experimental
import scala.concurrent.*
import scala.concurrent.duration.*
import scala.concurrent.ExecutionContext.Implicits.global

@experimental
object Example3 {

    def fetch(x:String)(using CpsDirect[Future]): String =
      s"${x}:ok"

    def fetchList(urls:List[String])(using CpsDirect[Future]): List[String] =
      urls.map(fetch)

    def main(args:Array[String]):Unit =
      val fr = async[Future] {
        fetchList(List("a","b"))
      }
      val r = Await.result(fr,1.second)
      println(r.mkString(","))

}