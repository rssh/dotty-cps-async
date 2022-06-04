package futureScope.examples

import scala.concurrent.*

import cps.*
import cps.monads.{*,given}
import futureScope.*

object TenUrls {

  import scala.concurrent.ExecutionContext.Implicits.global


  def readFirstN(networkApi: NetworkApi, urls: Seq[String], n:Int)(using ctx:FutureScopeContext): Future[Seq[String]] = 
    async[Future].in(Scope.child(ctx)) {
      val all = FutureGroup.collect( urls.map(url =>  networkApi.fetch(url)) )
      val successful = all.events.inTry.filter(_.isSuccess).take[Seq](n)
      await(successful).map(_.get)
    }
  

  trait NetworkApi {

    def fetch(url: String)(using ctx: FutureScopeContext): Future[String] 

 }


}