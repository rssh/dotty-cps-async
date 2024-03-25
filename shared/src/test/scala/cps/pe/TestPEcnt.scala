package cps.pe

import org.junit.{Test,Ignore}
import org.junit.Assert.*

import scala.util.*

import cps.*
import cps.util.FutureCompleter
import cps.testconfig.given



import scala.concurrent.ExecutionContext.Implicits.global

class TestPECnt:

  def qqq:Int = 0

  val LOG_MOD = 10
  val LOG_TRESHOLD = 100
  
  
  //implicit val printCode: cps.macroFlags.PrintCode.type = cps.macroFlags.PrintCode
  //implicit val printTree = cps.macroFlags.PrintTree
  //implicit inline def debugLevel: cps.macroFlags.DebugLevel = cps.macroFlags.DebugLevel(10)

  // automatic coloring is deleted.
  //   rewrite test withoput one with async/awaut.
  //   example with direct style can be found in compiler plugin tests
  def cntNoAutomaticColoring(counter: PEIntRef): PureEffect[PEToyLogger] = async[PureEffect]{
    val log = await(PEToyLogger.make())
    val value = await(counter.increment())
    if value % LOG_MOD == 0 then
       await(log.log(s"counter value = ${value}"))
    if (value - 1 == LOG_TRESHOLD) then  
       // Conversion will not be appliyed for == . For this example we want automatic conversion, so -1
       await(log.log("counter TRESHOLD"))
    log
  }


  @Test def peCnt_no_automatic_coloring9(): Unit =
     val counter = new PEIntRef(9)
     val c = cntNoAutomaticColoring(counter)
     val future = c.unsafeRunFuture().map{ log =>
       //println(s"PE:cnt_automatic_coloring, log.__all=${log.__all()} counter.get()=${counter.__get()} ")
       assert(log.__all().size == 1)
       assert(counter.__get() == 10)
     }
     FutureCompleter(future)


  @Test def peCnt_no_automatic_coloring10() =
     val counter = new PEIntRef(10)
     val c = cntNoAutomaticColoring(counter)
     val future = c.unsafeRunFuture().map{ log =>
       assert(log.__all().size == 0)
       assert(counter.__get() == 11)
     }
     FutureCompleter(future)

