package futureScope.examples

import java.net.*
import java.util.concurrent.ConcurrentLinkedQueue
import java.util.concurrent.atomic.AtomicInteger
import java.io.IOException
import scala.concurrent.*
import scala.concurrent.duration.*
import scala.util.*
import scala.util.control.NonFatal

import futureScope.*
import cps.*
import cps.monads.{*,given}

/**
 * Model for implementation if happy eyeball algorithm (see RFC 8305)
 **/
object HappyEyeballs2 {

    import scala.concurrent.ExecutionContext.Implicits.global

    enum AddressFamily {
      case Ip4, Ip6
    }
    
    trait NetworkAPI {
      def dnsResolve(name:String, family: AddressFamily): Future[List[InetAddress]]
      def familyOf(addr: InetAddress): AddressFamily
      def openSocket(addr: InetAddress): Future[Socket] 
    }

    case class Config(
      val resolutionDelay: FiniteDuration = 50.milliseconds,
      val connectionAttemptDelay: FiniteDuration = 250.milliseconds,
      val openConnectionTimeout: FiniteDuration = 30.seconds
    )

    enum Event {
      case DnsResolutionFailed
      case StartConnectionAttempt
      //case OpenConnectionTimeout
    }

    /**
     * return resolving
     **/
    def runResolving(networkApi: NetworkAPI, name: String, eventFlow: EventFlow[Event], config: Config)(using FutureScopeContext): ConcurrentLinkedQueue[InetAddress] = {
        val ip6Addrs = networkApi.dnsResolve(name,AddressFamily.Ip6)
        val ip4Addrs = networkApi.dnsResolve(name,AddressFamily.Ip4)
        val retval = new ConcurrentLinkedQueue[InetAddress]()
        val nFailures = new AtomicInteger(0)
        
        def handleResolveFailure(name:String, ex: Throwable): Unit = {
          println(s"Can't resolve name $name: ${ex.getMessage}")
          if (nFailures.incrementAndGet()==2) {
            eventFlow.post(Event.DnsResolutionFailed)
            eventFlow.finish()
          }
        }

        
        FutureScope.spawnAsync{
          async[Future] {
            try 
              val addrs = await(ip6Addrs)
              for{ a <- addrs} retval.add(a) 
              eventFlow.post(Event.StartConnectionAttempt)
            catch
              case NonFatal(ex) => handleResolveFailure(name,ex)
          }
        }
        val resolveDelay = FutureScope.spawnDelay(config.resolutionDelay)
        FutureScope.spawnAsync{ 
          async[Future]{
            try  
              val addrs = await(ip4Addrs)
              await(resolveDelay)
              for{ a <- addrs} retval.add(a) 
              eventFlow.post(Event.StartConnectionAttempt)
            catch
              case NonFatal(ex) => handleResolveFailure(name, ex)
          }
        }
        retval
    } 

   
    def openConnection(networkApi: NetworkAPI, name: String, config: Config): Future[Socket] = async[Future].in(Scope){     
      val result: Promise[Socket] = Promise()
      val eventFlow = EventFlow[Event]()
      result.completeWith(FutureScope.spawnTimeout(config.openConnectionTimeout))
      val inetAdresses = runResolving(networkApi,name, eventFlow, config)
      while(!result.isCompleted) {
          await(eventFlow.events.next) match
            case None =>
              result.tryFailure(new IOException("End of resolve events: all attempts are faoled"))
            case Some(event) =>
              event match
                case Event.StartConnectionAttempt =>
                   while(!inetAdresses.isEmpty && !result.isCompleted) {
                      val addr = inetAdresses.poll().nn
                      FutureScope.spawn{
                        try
                          val socket = await(networkApi.openSocket(addr))
                          if (! result.trySuccess(socket) ) then
                            socket.close()
                        catch
                          case NonFatal(ex) =>
                            println(s"error during opening connection to ${addr}: ${ex.getMessage()}")
                      }
                      await(FutureScope.spawnDelay(config.connectionAttemptDelay))
                   }
                case Event.DnsResolutionFailed =>
                   result.tryFailure(new IOException("Dns resolution failed"))  
      }
      await(result.future)
    }
    

}