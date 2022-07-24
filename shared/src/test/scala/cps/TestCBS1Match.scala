package cps

import org.junit.{Test,Ignore}
import org.junit.Assert._

import scala.quoted._
import scala.util.Success

import cps.testconfig.given

class TestBS1Match:


  @Test def tMatchC1_00(): Unit = 
     val c = async[ComputationBound]{
       10 match {
         case 1 => 3
         case _ => 100
       }
     }
     assert(c.run() == Success(100))

  @Test def tMatchC1_10(): Unit = 
     //implicit val printCode = cps.macroFlags.PrintCode
     val c = async[ComputationBound]{
       await(T1.cbi(1)) match {
         case 1 => 3
         case _ => 101
       }
     }
     assert(c.run() == Success(3))

  @Test def tMatchC1_01(): Unit = 
     val c = async[ComputationBound]{
       1 match {
         case 1 => await(T1.cbi(4))
         case _ => 101
       }
     }
     assert(c.run() == Success(4))

  @Test def tMatchC1_11(): Unit = 
     //implicit val printCode = cps.macroFlags.PrintCode
     val c = async[ComputationBound]{
       await(T1.cbi(2)) match 
         case 1 => await(T1.cbi(4))
         case 2 => await(T1.cbi(8))
         case _ => 101
     }
     assert(c.run() == Success(8))


  @Test def tMatchBind_01(): Unit = 
     //implicit val printCode = cps.macroFlags.PrintCode
     val c = async[ComputationBound]{
       val x = 2
       2 match 
         case x if x < 2 => await(T1.cbi(4))
         case y if y == 2 => await(T1.cbi(8))
         case _ => 101
     }
     assert(c.run() == Success(8))


