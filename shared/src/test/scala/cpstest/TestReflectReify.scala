package cpstest

import org.junit.{Test,Ignore}
import org.junit.Assert._

import scala.util.*


import cps.*
import cps.monads.{*,given}

import cps.testconfig.given
import cps.util.FutureCompleter


class TestReflectReify {


  @Test 
  def testReifyReflectOnComputationBound() = {
     val c = reify[ComputationBound] {
         val x = reflect(T1.cbi(1)) + reflect(T1.cbi(2))
         x
     }
     val r = c.run()
     assert(r.isSuccess)
     r match
       case Success(v) => assert(v == 3)
       case Failure(ex) => throw ex
  }

  
  @Test 
  def testReifyReflectOnList() = {
     val c = reify[List] {
         val x = reflect(List(1,2,3,5))
         val y = reflect(List(1,11))
         x*y
     }
     assert(c == List(1,11,2,22,3,33,5,55))
  }
  



}

