package cps

import org.junit.{Test,Ignore}
import org.junit.Assert._

import scala.quoted._
import scala.util.Success

import cps.testconfig.given


class TestBS1Option:

  @Test def optionGetOrElse_0(): Unit = 
     val c = async{
        var x = 1
        val y: Option[Int] = Some(1)
        var z = y.getOrElse({ x=x+1; await(T1.cbi(2)) })
        x
     }
     assert(c.run() == Success(1))

  @Test def optionGetOrElse_1(): Unit = 
     val c = async{
        var x = 1
        val y: Option[Int] = None
        var z = y.getOrElse({ x=x+1; await(T1.cbi(2)) })
        x
     }
     assert(c.run() == Success(2))

  @Test def optionGetOrElse_2_lv(): Unit = 
     val c = async{
        var x = 1
        val y: Option[Int] = None
        var z = y.getOrElse{
          val x1 = x + 1;
          x=x1; 
          await(T1.cbi(2)) 
        }
        x
     }
     assert(c.run() == Success(2))



