package cps

import org.junit.{Test,Ignore}
import org.junit.Assert._

import scala.quoted._
import scala.util.Success

import cps.testconfig.given


class TestCBS1Import:


  @Test def import_00(): Unit = 
     val c = async[ComputationBound]{
        var x = 1
        import scala.collection.mutable.Queue
        val q = Queue.empty
        2
     }
     assert(c.run() == Success(2))



