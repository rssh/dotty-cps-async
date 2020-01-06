package cps

import org.junit.{Test,Ignore}
import org.junit.Assert._

import scala.quoted._
import scala.util.Success


class TestBS1While


 // Dotty crash.
 // TODO: minimize and submit bug.
 //
  @Test def tWhileC1_11(): Unit = 
     val c = Async.transform[ComputationBound,Int]{
        val n = 10
        var s = 0
        var i = 0
        while(await(T1.cbBool(i < n))) {
          val q = await(T1.cbi(i))
          s += q
          i += 1
        }
        s
     }
     assert(c.run() == Success(45))



