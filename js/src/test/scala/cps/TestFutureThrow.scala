package cps

import cps.monads.given

import org.junit.{Test,Ignore}
import org.junit.Assert._

import scala.concurrent._
import scala.concurrent.duration._
import scala.language.postfixOps
import scala.quoted._
import scala.util.Success
import scala.util.Failure
import scala.util.Try

import java.util.Timer
import java.util.TimerTask

import cps.testconfig.given

class TestFutureThrow :

  import scala.concurrent.ExecutionContext.Implicits.global 

  @Test def throw1(): Future[Try[Unit]] = 
     val c = async[Future]{ 
       throw new RuntimeException("MyCustomException")
     }
     c.transform{
       case Failure(ex) =>
          if (ex.getMessage() == "MyCustomException")
            Success(())
          else
            Failure(ex)
       case Success(a) =>
           assert(""  == "Exceptio should be thrown")
           Failure(new RuntimeException("Exception should be thrown"))
     }.map(x => Success(x))
     

  @Test def throw2rethrow(): Future[Try[Unit]] = 
     val c = async[Future]{ 
       try {
         throw new RuntimeException("MyCustomException")
       }catch{
         case ex: RuntimeException =>
           throw ex
       }
     }
     c.transform{
       case Failure(ex) =>
          if (ex.getMessage() == "MyCustomException")
            Success(())
          else
            Failure(ex)
       case Success(a) =>
           assert(""  == "Exceptio should be thrown")
           Failure(new RuntimeException("Exception should be thrown"))
     }.map(x => Success(x))
     


     
