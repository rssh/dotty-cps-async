package cps

import org.junit.{Test,Ignore}
import org.junit.Assert._

import scala.util._


class TestBS1ShiftUsing:

  class TestResource(val label: String):
    var isClosed: Boolean = false
    var logLines: List[String] = List.empty
    def log(msg:String):Unit =
         logLines = msg :: logLines
    def close(): Unit  =
      this.isClosed = true

  given Using.Releasable[TestResource]:
    def release(resource: TestResource):Unit =
       resource.close()
  

  @Test def testUsingResource1(): Unit = 
     implicit val printCode = cps.macroFlags.PrintCode
     implicit val debugLevel = cps.macroFlags.DebugLevel(15)
     val c = async[ComputationBound]{
         val r = new TestResource("t1")
         Using.resource(r){ r =>
             val q = "here: " + await(T1.cbs(r.label))
             r.log(q)
         }
         r.isClosed
     }
     assert(c.run() == Success(true))





