package cps.vp

import cps.*
import cps.monads.{*,given}

import scala.concurrent.*
import scala.concurrent.ExecutionContext.Implicits.global

object VP1:

   /*
    * compiler crash in 3.1.0
    * see https://github.com/lampepfl/dotty/issues/13809
    */
   def allocateServiceOperator(optInUsername: Option[String]): Future[Unit] = async[Future] {
      val username = optInUsername match
         case None =>
            while(false) {
               val nextResult = await(op1("NQ"))
               val countResult = await(op1("CQ"))
            }
         case Some(inUsername) =>
            val x = await(op1("SQ"))
            inUsername
   }
   

   def op1(query: String): Future[String] = ???




