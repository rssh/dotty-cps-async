package cpstest

import cps.*
import cps.monads.{*,given}
import cps.testconfig.given
import cps.util.FutureCompleter

import scala.concurrent.*
import scala.concurrent.duration.*
import scala.concurrent.ExecutionContext.Implicits.global


import org.junit.{Test,Ignore}


case class NameValueDTO(name: String, value:String)

case class WebhookCredentialRequestedValuesDTO(
                                               schemaId: String,
                                               credWalletId: String,
                                               fields: Seq[NameValueDTO],
                                              )



case class WebhookActionEventDTO(protocolVersion: Int,
                                 publicServiceDid: String,
                                 actionParams: Seq[NameValueDTO],
                                 receivedCredentials: Seq[WebhookCredentialRequestedValuesDTO],
                                )

class TestCompileWebhookActionEventDTO {

   def  fsuccess[A](a:A): Future[A] =
     Future successful a


   @Test
   def testCompileWebhookActionEventDTO() = {

     //implicit val printCode = cps.macros.flags.PrintCode
     //implicit val printTree = cps.macros.flags.PrintTree
     //implicit val debugLevel = cps.macros.flags.DebugLevel(5)
     val retval = async[Future] {
       val one = await(fsuccess(1))
       val eventDTO = WebhookActionEventDTO(
        protocolVersion = 3,
        publicServiceDid = "publicServiceDid",
        //actionEventId = "1",
        actionParams = Seq.empty,
        receivedCredentials = Seq(
                WebhookCredentialRequestedValuesDTO(
                   schemaId = "loginSchemaId",
                   credWalletId = "1",
                   fields = Seq(NameValueDTO("code", "123456"))
                ),
                WebhookCredentialRequestedValuesDTO(
                   schemaId = "trustedEmailSchemaId",
                   fields = Seq(NameValueDTO("email", "me@example.com")),
                   credWalletId = "2",
                )
        ),
       )
       val vevent = await(fsuccess(eventDTO))
       assert(vevent.receivedCredentials.nonEmpty)
       vevent
     }
     FutureCompleter(retval)
   }


}
