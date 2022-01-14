package cps.gopherlike

import org.junit.{Test,Ignore}
import org.junit.Assert._

import scala.quoted._
import scala.concurrent.Future
import scala.util.Success

import cps._

class TestFM1:


  class FlatMapped[F[_]:CpsSchedulingMonad,A,B](prev: IFReader[F,A], f: A=>IFReader[F,B])  {
     val bChannel = new CIFChannel[F,B]()

     def run(): F[Unit] = 
      //implicit val printCode = cps.macroFlags.PrintCode
      //implicit val printTree = cps.macroFlags.PrintTree
      //implicit val debugLevel = cps.macroFlags.DebugLevel(20)
      async[F]{
        while{
          prev.optRead() match
            case Some(a) =>
              //val internal = f(a)
              //while{
              //  internal.optRead() match
              //    case Some(b) => 
              //      bChannel.write(b)
              //      true
              //    case None =>
              //      false
              //} do ()
              true
            case None =>
              false
        } do ()
     }

   }

 
   @Test def reproduce(): Unit = 
         assert(true)

  


