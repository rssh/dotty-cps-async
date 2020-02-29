package cps

import org.junit.{Test,Ignore}
import org.junit.Assert._

import scala.quoted._
import scala.util.Success


class TestBS1Match:


  @Test def tMatchC1_00(): Unit = 
     val c = async[ComputationBound]{
       10 match {
         case 1 => 3
         case _ => 100
       }
     }
     assert(c.run() == Success(100))



