package futureScope

import scala.concurrent.*

import cps.*
import cps.monads.{*,given}


object TenUrls {

  import scala.concurrent.ExecutionContext.Implicits.global


  def readFirstN(urls: Seq[String], n:Int): Future[Seq[String]] = async[Future].in(Scope) {
    // TODO: try to use apply.
    val all = FutureGroup.collect( urls.map(url =>  NetworkApi.fetch(url)) )
    val successful = all.events.inTry.filter(_.isSuccess).take[Seq](n)
    await(successful).map(_.get)
  }
  

  object NetworkApi {

    def fetch(url: String)(using ctx: FutureScopeContext): Future[String] = {
      // actual implementation will run fetch abd cancelled after context will be cancelled
      Future successful (s"data $url")
    }

 }


}