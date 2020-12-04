package cps.utestlike


import org.junit.{Test,Ignore}
import org.junit.Assert._

import scala.quoted._
import scala.util.Success

import cps._

class TestUtestLike:

  @Test def reproduce(): Unit =
     val f = T1.cbi(10)
     val c = async{
        val r1 = await(f)
        UtestLikeMacro.assert( r1 == 2 )
     }
     assert(c.run() == Success(()))

