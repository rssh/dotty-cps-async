package cps.streamlike

import org.junit.{Test,Ignore}
import org.junit.Assert._


import cps._
import cps.testconfig.given


class TestStreamMonad:

  @Test def streamReadWrite(): Unit = 
     val in1 = LazyStream(1,2,3)
     val in2 = LazyStream(4,5,6)
     val c = async[LazyStream] {
        val x = await(in1)
        val y = await(in2)
        (x,y)
     }
     assert(c.head == (1,4))
     val c1 = c.tail
     assert(c1.head == (1,5))
     val c2 = c1.tail
     assert(c2.head == (1,6))
     val c3 = c2.tail
     assert(c3.head == (2,4))




