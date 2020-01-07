package cps

import org.junit.{Test,Ignore}
import org.junit.Assert._

import scala.quoted._
import scala.util.Success


class TestBS1Try


  @Test def try_00n(): Unit = 
     val c = async[ComputationBound]{
        var x = 1
        try {
          //println(10)
          x = 2
        }catch{
          case ex:Exception => ex.printStackTrace()
          x = 3
        }
        x
     }
     assert(c.run() == Success(2))





