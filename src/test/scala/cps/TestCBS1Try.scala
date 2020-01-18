package cps

import org.junit.{Test,Ignore}
import org.junit.Assert._

import scala.quoted._
import scala.util.Success


class TestBS1Try


  @Test def try_00n_p(): Unit = 
     val c = async[ComputationBound]{
        var x = 1
        try {
          //println(10)
          x = 2
        }catch{
          case ex:Exception => //ex.printStackTrace()
          x = 3
        }
        x
     }
     assert(c.run() == Success(2))


  @Test def try_00n_f(): Unit = 
     val c = async[ComputationBound]{
        var x = 1
        try {
          //println(10)
          throw new RuntimeException("AAA")
          x = 2
        }catch{
          case ex:Exception => // ex.printStackTrace()
          x = 3
        }
        x
     }
     assert(c.run() == Success(3))



