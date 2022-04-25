package futureScope

import scala.concurrent.*

import cps.*
import cps.monads.{*,given}


object TenUrls {

/*
  def readFirstN(urls: Seq[String], n:Int): Future[Seq[String]] = async[Future].in(using Scope) {
    val all = FutureGroup( urls.map(url =>  NetworkApi.fetch(url)) )
    all.events.filter()
    while { 
        await(all.events.next) match
          case Some(data) =>
            retval = retval :+ data
            retval.size < n 
          case None => false
    } do ()
    retval
  }
  */

  object NetworkApi {

    def fetch(url: String)(using ctx: FutureScopeContext): Future[String] = {
      Future successful (s"data $url")
    }

 }


}