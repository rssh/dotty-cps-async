package cps

import org.junit.{Test,Ignore}
import org.junit.Assert._

import scala.quoted._
import scala.util.Success


class TestBS1

  @Test def testBadTree(): Unit =
    val c = B.badTree[Int]{
         val x1 = 3
         val x2 = 4 
         CBM.pure(x1 + x2)
    }



