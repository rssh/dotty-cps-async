package cps

import org.junit.{Test,Ignore}
import org.junit.Assert._

import scala.util._
import cps.testconfig.given

import cps.plugin.annotation.CpsDebugLevel

@CpsDebugLevel(20)
class TestBS1ShiftUsing:

  class TestResource(val label: String):
    var isClosed: Boolean = false
    var logLines: List[String] = List.empty
    def log(msg:String):Unit =
         logLines = msg :: logLines
    def close(): Unit  =
      this.isClosed = true

  given Using.Releasable[TestResource] with
    def release(resource: TestResource):Unit =
       resource.close()


  @Test def testUsingResources2Complex(): Unit =
     //implicit val printCode = cps.macroFlags.PrintCode
     //implicit val debugLevel = cps.macroFlags.DebugLevel(20)
     var svR2: Option[TestResource] = None;
     val c = async[ComputationBound]{
         val r1 = new TestResource("r1")
         Using.resources(r1, new TestResource("r2")){ (r1, r2) =>
             val q =  await(T1.cbs(r1.label)) + await(T1.cbs(r2.label))
             //r1.log(q)
             svR2 = Some(r2)
             // TODO: bug in dotty during unpickling.
             //r2.isClosed
             (r2.isClosed: Boolean)
         }
     }
     assert(c.run() == Success(false))
     assert(svR2.get.isClosed == true)






