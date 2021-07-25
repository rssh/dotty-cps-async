package cps

import org.junit.{Test,Ignore}
import org.junit.Assert.*

import scala.util.*

import cps.automaticColoring.{*,given}
import scala.language.implicitConversions

import java.util.concurrent.atomic.*

class TestCBS2ACCnt:

  def qqq:Int = 0

  val LOG_MOD = 10
  val LOG_TRESHOLD = 100
  
  def createCounter(n:Int) = new AtomicInteger(n)
  
  //implicit val printCode: cps.macroFlags.PrintCode.type = cps.macroFlags.PrintCode
  //implicit val printTree = cps.macroFlags.PrintTree
  //implicit val debugLevel = cps.macroFlags.DebugLevel(20)

  def increment(cnt: AtomicInteger): ComputationBound[Int] =
    Thunk( () => ComputationBound.pure(cnt.incrementAndGet()) )

  class Log:
    private var lines = Vector[String]()

    def log(msg:String): Unit =
       lines = lines :+ msg

    def all: Vector[String] = lines


  def cntAutomaticColoring(counter: AtomicInteger): ComputationBound[Log] = async[ComputationBound]{
    val log = new Log
    val value = increment(counter) 
    if value % LOG_MOD == 0 then
       log.log(s"counter value = ${await(value)}")
    if (value - 1 == LOG_TRESHOLD) then  
       // Conversion will not be appliyed for == . For this example we want automatic conversion, so -1
       log.log("counter TRESHOLD")
    log
  }

  def cntNoAutomaticColoring(counter: AtomicInteger): ComputationBound[Log] = async[ComputationBound]{
    val log = new Log
    val value = await(increment(counter))
    if value % LOG_MOD == 0 then
       log.log("counter value = ${value}")
    if value-1 == LOG_TRESHOLD then
       log.log("counter TRESHOLD")
    log
  }


  @Test def cnt_automatic_coloring(): Unit = 
     val counter = createCounter(9)
     val c = cntAutomaticColoring(counter)
     val r: Try[Log] = c.run()
     //println(s"cn_automatic_coloring, r=$r, r.get.all=${r.get.all} counter.get()=${counter.get()} ")
     assert(r.isSuccess)
     assert(r.get.all.size == 1)
     assert(counter.get() == 10)


