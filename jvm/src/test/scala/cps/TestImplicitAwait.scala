package cps

import cps.testconfig.given

import org.junit.{Test,Ignore}
import org.junit.Assert._

import scala.concurrent._
import scala.concurrent.duration._
import scala.language.postfixOps
import scala.language.implicitConversions
import scala.quoted._
import scala.util.Success

import cps.monads.given


class TestImplicitAwait:

  import scala.concurrent.ExecutionContext.Implicits.global 

  object ApiEmulator:
     def fetchUrl(url: String): Future[String] = 
           Future successful "TEXT"
     def classifyText(text: String): Future[Int] = 
           Future successful 0
     def retrieveDMPInfo(url:String, theme: Int, userId: String): Future[String] =
           Future successful s"${url}:${theme}:${userId}"
  

  @Test def withImplicitAwait(): Unit = 
     import cps.automaticColoring.{*,given}
     //implicit val printCode = cps.macroFlags.PrintCode
     //implicit val printTree = cps.macroFlags.PrintTree
     //implicit val debugLevel = cps.macroFlags.DebugLevel(20)
     val api = ApiEmulator
     val c = async[Future]{ 
        val url = "http://www.example.com"
        val data = api.fetchUrl("http://www.example.com")
        val theme = api.classifyText(data)
        val dmpInfo: String = api.retrieveDMPInfo(url, theme, "1")
        dmpInfo
     }
     val r = Await.result(c, 10 seconds)
     assert(r=="http://www.example.com:0:1")



  @Test def withExplicitAwait(): Unit = 
     //implicit val printCode = cps.macroFlags.PrintCode
     //implicit val debugLevel = cps.macroFlags.DebugLevel(20)
     val api = ApiEmulator
     val c = async[Future]{ 
        val url = "http://www.example.com"
        val data = await(api.fetchUrl("http://www.example.com"))
        val theme = api.classifyText(data)
        val dmpInfo: String = await(api.retrieveDMPInfo(url, await(theme), "1"))
        dmpInfo
     }
     val r = Await.result(c, 10 seconds)
     assert(r=="http://www.example.com:0:1")



