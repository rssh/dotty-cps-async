package cpstest


import cps._
import scala.annotation.experimental
import scala.concurrent._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

import java.util.concurrent.ScheduledExecutorService

@experimental
object Test11m1 {

  val scheduledExecutorService = ScheduledExecutorService.newScheduledThreadPool(1)

  def fetch(url:String, delay: Long)(using CpsDirect[Future]): String = {
      val p = Promise[String]()
      scheduledExecutorService.schedule(
        () => p.success(s"result of $url: $delay"),
        delay,
        TimeUnit.MILLISECONDS)
      await(p.future)
  }

  def fetchAll(urls:List[String], delays:List[Long])(using CpsDirect[Future]): List[String] = {
    urls.zip(delays).delay.map(async((url,delay) => fetch(url,delay))).map(await(_))
  }


}

