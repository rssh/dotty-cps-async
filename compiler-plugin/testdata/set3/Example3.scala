package cpstest

import cps.*
import cps.monads.{*,given}
import scala.concurrent.Future

object Example3 {

    def fetch(x:String)(using CpsMonadContext[Future]): String =
      Future successful s"${x}:ok"

    def fetchList(urls:List[String])(using CpsMonadContext[Future]): List[String] =
      urls.map(fetch)

    def main(args:Array[String]):Unit =
      val r = fetchList(List("a","b"))
      println(r.mkString(","))

}