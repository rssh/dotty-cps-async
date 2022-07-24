package cps

import org.junit.{Test,Ignore}
import org.junit.Assert._

import scala.util._
import cps.testconfig.given


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


  @Test def testUsingResource1(): Unit =
     //implicit val printCode = cps.macroFlags.PrintCode
     //implicit val debugLevel = cps.macroFlags.DebugLevel(15)
     val c = async[ComputationBound]{
         val r = new TestResource("t1")
         Using.resource(r){ r =>
             val q = "here: " + await(T1.cbs(r.label))
             r.log(q)
         }
         r.isClosed
     }
     assert(c.run() == Success(true))


  @Test def testUsingResource1throw(): Unit =
     //implicit val printCode = cps.macroFlags.PrintCode
     //implicit val debugLevel = cps.macroFlags.DebugLevel(15)
     val r = new TestResource("t2")
     val c = async[ComputationBound]{
         Using.resource(r){ r =>
             val q = "here: " + await(T1.cbs(r.label))
             throw new RuntimeException("testUsingResource1throw")
             r.log(q)
         }
     }
     assert(c.run().isFailure)
     assert(r.isClosed)

  @Test def testUsingResources2(): Unit =
     //implicit val printCode = cps.macroFlags.PrintCode
     //implicit val debugLevel = cps.macroFlags.DebugLevel(15)
     var svR2: Option[TestResource] = None;
     val c = async[ComputationBound]{
         val r1 = new TestResource("r1")
         Using.resources(r1, new TestResource("r2")){ (r1, r2) =>
             svR2 = Some(r2)
             r2.isClosed
         }
     }
     assert(c.run() == Success(false))
     assert(svR2.get.isClosed == true)

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

  @Test def testUsingResources3(): Unit =
     //implicit val printCode = cps.macroFlags.PrintCode
     //implicit val debugLevel = cps.macroFlags.DebugLevel(15)
     var svR2: Option[TestResource] = None;
     var svR3: Option[TestResource] = None;
     val c = async[ComputationBound]{
         val r1 = new TestResource("r1")
         val r3 = new TestResource("r3")
         Using.resources(r1, new TestResource("r2"), r3){ (r1, r2, r3) => 
             svR2 = Some(r2)
             svR3 = Some(r3)
             await(T1.cbs(r1.label)) + r2.label + r3.label
         }
     }
     assert(c.run() == Success("r1r2r3"))
     assert(svR2.get.isClosed == true)
     assert(svR3.get.isClosed == true)


  @Test def testUsingResources4(): Unit =
     var svR2: Option[TestResource] = None;
     var svR3: Option[TestResource] = None;
     var svR4: Option[TestResource] = None;
     val c = async[ComputationBound]{
         val r1 = new TestResource("r1")
         val r3 = new TestResource("r3")
         val r4 = new TestResource("r4")
         Using.resources(r1, new TestResource("r2"), r3, r4){ (r1, r2, r3, r4) => 
             svR2 = Some(r2)
             svR3 = Some(r3)
             svR4 = Some(r4)
             await(T1.cbs(r1.label)) + r2.label + r3.label + r4.label
         }
     }
     assert(c.run() == Success("r1r2r3r4"))
     assert(svR2.get.isClosed == true)
     assert(svR3.get.isClosed == true)
     assert(svR4.get.isClosed == true)





