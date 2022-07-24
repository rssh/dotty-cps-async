package cps.pe

import org.junit.{Test,Ignore}
import org.junit.Assert._

import scala.quoted._
import scala.util.Success

import cps._
import cps.util.FutureCompleter
import cps.testconfig.given

import scala.concurrent.ExecutionContext.Implicits.global

class TestPE1:

  def qqq = 1
   
  @Test def testPureEffectConstant() = 
     val c = async[PureEffect](3)
     val future = c.unsafeRunFuture().map(x => assert(x == 3))
     FutureCompleter(future)

  @Test def tBlockNoAsync(): Unit = 
     val c = async[PureEffect]{
         val x1 = 3
         val x2 = 4 //await(T1.cbi(4))
         x1 + x2
         //7
     }
     val future = c.unsafeRunFuture().map(x => assert(x == 7))
     FutureCompleter(future)



