package cpstest

import cps.*

import cps.{ComputationBound, Thunk}
import org.junit.{Ignore, Test}
import org.junit.Assert.*

import scala.annotation.experimental
import scala.util.*
import java.util.concurrent.atomic.*
import cps.testconfig.given

// rewrited with CpsDirect instead automatic coloring
@experimental
class TestCBS2ACCntDirect:

  def qqq:Int = 0

  val LOG_MOD = 10
  val LOG_TRESHOLD = 100

  def createCounter(n:Int) = new AtomicInteger(n)

  //  disable loom, to prevent compiler crash
  given noLoom1: cps.macros.flags.UseLoomAwait.type = ???
  given noLoom2: cps.macros.flags.UseLoomAwait.type = ???


  //implicit val printCode: cps.macroFlags.PrintCode.type = cps.macroFlags.PrintCode
  //implicit val printTree = cps.macroFlags.PrintTree
  //implicit val debugLevel = cps.macroFlags.DebugLevel(20)

  def increment(cnt: AtomicInteger)(using CpsDirect[ComputationBound]): Int =
    val cb: ComputationBound[Int] = Thunk( () => ComputationBound.pure(cnt.incrementAndGet()) )
    await(cb)

  class Log:
    private var lines = Vector[String]()

    def log(msg:String): Unit =
      lines = lines :+ msg

    def all: Vector[String] = lines


  def cntDirect(counter: AtomicInteger): ComputationBound[Log] = async[ComputationBound]{
    val log = new Log
    val value = increment(counter)
    if value % LOG_MOD == 0 then
      log.log(s"counter value = ${value}")
    if (value - 1 == LOG_TRESHOLD) then
    // Conversion will not be appliyed for == . For this example we want automatic conversion, so -1
      log.log("counter TRESHOLD")
    log
  }


  @Test def cnt_direct(): Unit =
    val counter = createCounter(9)
    val c = cntDirect(counter)
    val r: Try[Log] = c.run()
    //println(s"cn_automatic_coloring, r=$r, r.get.all=${r.get.all} counter.get()=${counter.get()} ")
    assert(r.isSuccess, "r should be success")
    assert(r.get.all.size == 1, "r.get.all.size==1")
    assert(counter.get() == 10, "counter.get() == 10")


