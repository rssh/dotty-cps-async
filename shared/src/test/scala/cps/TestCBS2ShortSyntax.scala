package cps

import org.junit.{Test,Ignore}
import org.junit.Assert.*

import scala.util.*
import cps.syntax.*


class TestCBS2ShortSyntax:

  
  @Test def testShortSynax(): Unit = 
     //implicit val printCode = cps.macroFlags.PrintCode
     val c = async {
         val x = !T1.cbi(2)
         val y = !T1.cbi(3)
         val z = !T1.cbi(4) + !T1.cbi(5) + 6
         x+y+z
     }
     val r = c.run()
     assert(r.isSuccess)
     r match
       case Success(v) => assert(v == 2+3+4+5+6)
       case Failure(ex) => throw ex



