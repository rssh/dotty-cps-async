package cpstest

import cps.*
import cps.monads.{*,given}
import scala.concurrent.Future

object Example3 {

    def fetch(x:String)(using CpsMonadContext[Future]): String =
      s"${x}:ok"

    def fetchList(urls:List[String])(using CpsMonadContext[Future]): List[String] =
      urls.map(fetch)

    def main(args:Array[String]):Unit =
      val fr = async[Future] {
        fetchList(List("a","b"))
      }
      val r = Await.result(fr)
      println(r.mkString(","))

}