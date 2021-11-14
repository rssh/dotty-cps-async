package cps.gopherlike

import org.junit.{Test,Ignore}
import org.junit.Assert._

import scala.quoted._
import scala.concurrent.Future
import scala.util.Success

import cps._
import cps.monads.FutureAsyncMonad
import scala.concurrent.ExecutionContext.Implicits.global

class SF3W1SelectLoop[F[_]](using val am:CpsMonad[F]):
        
  def fold[S](s0:S)(step: (S,SF3W1SelectLoop[F])=> S): S = 
   ???

  transparent inline def afold[S](inline s0:S)(inline step: (S,SF3W1SelectLoop[F]) => S): F[S] =
     async[F] { 
       fold(s0)(step)
     }


object SF3W1SelectLoop:
   def create[F[_]](using am:CpsMonad[F]) = SF3W1SelectLoop(using am)


class TestSF3W1:

  def qqq: Int = 0

  @Test def reproduce(): Unit = {
     //implicit val printCode = cps.macroFlags.PrintCode
     //implicit val printTree = cps.macroFlags.PrintTree
     //implicit val debugLevel = cps.macroFlags.DebugLevel(20)

     
     val select = SLSelectLoop.create[Future]

     val sf = select.afold((true)){ (x,s) =>
            x
         }
   
     assert(true)
  }
  



