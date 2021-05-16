package cps

import org.junit.{Test,Ignore}
import org.junit.Assert._

import scalajs.js.{Promise => JsPromise}
import scalajs.js.{Error => JsError}
import scalajs.js.{eval => jsEval}
import scala.scalajs.js.annotation._

import scala.concurrent.Future
import scala.util._

import scala.concurrent.ExecutionContext.Implicits.global

import cps.monads.given

class JSPromiseBasedTestAPI:

   def retrieveData(uri:String): JsPromise[String] =
     uri match
      case "good" => JsPromise.resolve("resolved-data")
      case "bad-special-case"=> JsPromise.reject("bad-special-case")
      case _  => 
        JsPromise.reject(new JsError("data is not good"))


trait FromScalaTestJSApi:

   @JSExport
   def myFunction: JsPromise[String]



class TestJsPromiseInterop:

  @Test def testGood(): Future[Try[Unit]] =
    val api = new JSPromiseBasedTestAPI()
    val check = async[Future]{
       val data = await(api.retrieveData("good"))
       data + "-22"
    }
    check.map{ v =>
       assert( v == "resolved-data-22" )
       Success(())
    }

  /*
  @Test def testGoodWithCB() =
    val check = async[ComputationBound] {
       val data = await(retrieveData)
       data + "-23"
    }
    val r = check.run()
    assert( r == Success("resolved-data-23") )
  */


  @Test def testEval() =
     val q = jsEval("1")
     assert(q == 1)



