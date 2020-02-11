package cps

import org.junit.{Test,Ignore}
import org.junit.Assert._

import scala.quoted._
import scala.util.Success


class TestBS1Throw:


  @Test def try_00n(): Unit = 
     val c = async[ComputationBound]{
        throw new RuntimeException("AAA")
     }
     assert(c.run().isFailure)





