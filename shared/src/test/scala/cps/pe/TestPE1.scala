package cps.pe

import org.junit.{Test,Ignore}
import org.junit.Assert._

import scala.quoted._
import scala.util.Success

import cps._
import cps.util.FutureCompleter

import scala.concurrent.ExecutionContext.Implicits.global

class TestPE1:

  def qqq = 1
   
  @Test def testPureEffectConstant() = 
     val c = async[PureEffect](3)
     println(s"c = $c")
     val future = c.unsafeRunFuture().map(x => assert(x == 3))
     FutureCompleter(future)

  /*
  @Test def tBlockNoAsync(): Unit = 
     val c = Async.async[ComputationBound]{
         val x1 = 3
         val x2 = 4 //await(T1.cbi(4))
         x1 + x2
         //7
     }
     val c1 = c.run()
     assert( c1 == Success(7) )
  */



