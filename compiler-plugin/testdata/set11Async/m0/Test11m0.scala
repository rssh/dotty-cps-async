package cpstest

import scala.annotation.experimental
import scala.concurrent._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

import cps.*
import cps.monads.{*,given}
import cps.plugin.annotation.CpsDebugLevel

import java.util.concurrent.ScheduledExecutorService


@experimental
@CpsDebugLevel(20)
object Test11m0 {


  def fetch(url:String)(using CpsDirect[Future]): String = {
     s"result of $url"
  }

  def fetchAll(urls:List[String])(using CpsDirect[Future]): List[String] = {
    urls.map(url => async[Future](fetch(url))).map(await(_))
  }

  def main(args:Array[String]): Unit = {
     val fr = async[Future] {
       fetchAll(List("url1", "url2", "url3"))
     }
     val r = Await.result(fr, 1.second)
     if (r.contains("result of url1") && r.contains("result of url2") && r.contains("result of url3")) then
       println("Ok")
     else
       println(r)
  }

}

