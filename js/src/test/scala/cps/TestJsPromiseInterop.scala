package cps

import scala.language.implicitConversions

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
import cps.monads.jsfuture.{given,*}

class JSPromiseBasedTestAPI:

   def retrieveData(uri:String): JsPromise[java.lang.String] =
     uri match
      case "good" => JsPromise.resolve("resolved-data")
      case "bad-special-case"=> JsPromise.reject("bad-special-case")
      case _  => 
        JsPromise.reject(new JsError("data is not good"))




@JSExportTopLevel("FromScalaExample1")
object FromScalaExample1:

   @JSExport
   def myFunction(x: String): JSFuture[String] = async[JSFuture] {
      val q1 = x
      if (x == "[]") then
        "[]"
      else
        await(Future successful s"[$q1]")
   }
   

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

  @Test def testRejectedPromise1(): Future[Try[Unit]] =
    val api = new JSPromiseBasedTestAPI()
    val check = async[Future]{
       try {
         await(api.retrieveData("bad"))
       } catch {
         case ex: Throwable =>
            ex.getMessage() 
       }
    }
    check.map{ v =>
       assert( v == "Error: data is not good" )
       Success(())
    }

   
  @Test def testGoodWithAutomaticColoring(): Future[Try[Unit]] =
    import cps.automaticColoring.given
    val api = new JSPromiseBasedTestAPI()
    val check = async[Future]{
       val data = api.retrieveData("good")
       data + "-23"
    }
    check.map{ v =>
       assert( v == "resolved-data-23" )
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

  @Test def testJSFuture(): Future[Try[Unit]] =
     val promiseApi = new JSPromiseBasedTestAPI()
     val check = async[JSFuture] {
       val x = 1;
       val xx = await(promiseApi.retrieveData("good"))
       s"${xx}-${x}"
     }
     check.future.map(x => {
       assert(x == "resolved-data-1")
     }).map(Success(_))

  @Test def testEvalNoAwait():Future[Try[Unit]] =
     val q = jsEval(
     """
       FromScalaExample1.myFunction("1")
     """
     )
     q match
       case p: scalajs.js.Promise[_] =>
          p.toFuture.map(x =>
               assert(x == "[1]")
          ).map(_ => Success(()))
       case other =>
          throw new RuntimeException("promise expected")


  @Test def testEvalWithJSAwait() =
     val q = jsEval(
     """
        async function testFun() {
           const two = await FromScalaExample1.myFunction("2")
           if (two == '[2]')
             return "ok"
           else
             return "bad:"+two
        }
        testFun()
     """
     )
     q match
       case p: scalajs.js.Promise[_] =>
          p.toFuture.map(x =>
               assert(x == "ok")
          ).map(_ => Success(()))
       case other =>
          throw new RuntimeException("promise expected")


