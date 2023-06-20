package cpstest


import cps.*
import cps.monads.{*,given}
import cps.plugin.annotation.CpsDebugLevel
import scala.annotation.experimental
import scala.concurrent.*
import scala.concurrent.duration.*
import scala.concurrent.ExecutionContext.Implicits.global

import java.util.concurrent.ScheduledExecutorService
import java.util.concurrent.Executors
import java.util.concurrent.TimeUnit

@experimental 
//@CpsDebugLevel(20)
object Test11m1 {

  val scheduledExecutorService = Executors.newScheduledThreadPool(1)

  def fetch(url:String, delay: Long)(using CpsDirect[Future]): String = {
      val p = Promise[String]()
      scheduledExecutorService.schedule(
        () => p.success(s"$url:$delay"),
        delay,
        TimeUnit.MILLISECONDS)
      val retval = await(p.future)
      retval
  }

  def fetchAll(urls:List[String], delays:List[Long])(using CpsDirect[Future]): List[String] = {
    urls.zip(delays).map((url,delay) => asynchronized(fetch(url,delay))).map(await(_))
  }

  def main(args: Array[String]): Unit = {
    val nMillis0 = System.currentTimeMillis()
    val fr = async[Future] {
      fetchAll(List("url1", "url2", "url3"),List(500L, 100L, 200L))
    }
    val r = Await.result(fr, 1.second)
    val nMillis1 = System.currentTimeMillis()
    val delta = nMillis1 - nMillis0
    if (r == List("url1:500","url2:100","url3:200") && delta < 800) then
      println("Ok")
    else
      println(r)
    scheduledExecutorService.shutdown()
  }

}

