package cps.pe

import org.junit.{Test,Ignore}
import org.junit.Assert._

import scala.quoted._
import scala.util.Success

import cps._
import cps.util.FutureCompleter

import scala.concurrent.ExecutionContext.Implicits.global

class TestPEcnt:

  val LOG_MOD=10
  val TRESHOLD = 100

  def runCounterTestV1(initVal:Int, logger: ToyLogger): PureEffect[Unit] = async[PureEffect] {
       val counter = await(PEIntRef.make(initVal))
       val value = await(counter.increment())
       if (value % LOG_MOD == 0) then
           logger.log(s"counter value is $value")
       if (value == TRESHOLD) then
           logger.log(s"counter exceed treshold")
  }

  @Test def testCntV1_0() = 
     val logger = new ToyLogger()
     val c = runCounterTestV1(0,logger)
     val future = c.unsafeRunFuture().map(_ => 
        assert(logger.lines.isEmpty)
     )
     FutureCompleter(future)




