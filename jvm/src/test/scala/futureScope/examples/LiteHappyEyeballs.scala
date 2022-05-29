package futureScope.examples

import java.net.*
import scala.concurrent.*
import scala.concurrent.duration.*
import scala.util.*
import scala.util.control.NonFatal

import futureScope.*
import cps.*
import cps.monads.{*,given}

/**
 * Model for implementation if happy eyeball algorithm (see RFC 8305)
 * in somplicified version, to allow dirext comparison wt
 **/
object LiteHappyEyeballs {

  import scala.concurrent.ExecutionContext.Implicits.global


  trait NetworkAPI {
    def dnsResolve(name:String): List[InetAddress]
    def openSocket(addr: InetAddress): Future[Socket] 
    //def establishConnection(socket: Socket): Future[Unit]
  }

  case class Config(
    val resolutionDelay: FiniteDuration = 50.milliseconds,
    val connectionAttemptDelay: FiniteDuration = 250.milliseconds,
    val openConnectionTimeout: FiniteDuration = 30.seconds
  )

  

  def openConnection(networkApi: NetworkAPI, name: String, config: Config): Future[Socket] = async[Future].in(Scope){
    val result = Promise[Socket]()
    var addresses = networkApi.dnsResolve(name).iterator
    while(addresses.hasNext && !result.future.isCompleted) {
      val addr = addresses.next
      FutureScope.spawn{
        try 
          val socket = await(networkApi.openSocket(addr))
          if (!result.trySuccess(socket)) {
            socket.close()
          }  
        catch
          case NonFatal(ex) =>
            println(s"Exception during trying connect to $addr ${ex.getMessage}")
      }
      FutureScope.timedAwaitCompleted(result.future,config.connectionAttemptDelay)
    }  
    await(result.future)
  }

}