package cps.pe

import scala.annotation.experimental
import scala.concurrent.*
import scala.concurrent.duration.*
import org.junit.{Test,Ignore}

import cps.*
//  import cps.testconfig.given


@experimental
class TestInlineOpInResourceM8 {

  import scala.concurrent.ExecutionContext.Implicits.global

  def openResource(name: String, logger: PEToyLogger): Resource[PureEffect,String] = {
    Resource.Allocate(() =>
      PureEffect.delay((name,
        () => async[PureEffect](logger.log(s"close $name"))))
    )
  }



  def openResourceDirect(name:String, logger: PEToyLogger)(using CpsDirect[[X]=>>Resource[PureEffect,X]]): String = {
     await(openResource(name,logger))
  }

  inline def doSomething(name: String, logger: PEToyLogger)(using CpsDirect[PureEffect]): String = {
    logger.log(s"doSomething(${name})")
    s"doSomething(${name})"
  }


  @Test
  def testResourceInteropWithInlineOp() = {
    val c = async[[X]=>>Resource[PureEffect,X]] {
              val logger = PEToyLogger.make()
              val r1 = await(openResource("r1", logger))
              logger.log("inside")
              val x = doSomething(r1, logger)
              logger
    }
    val c1 = Resource.run(c)
    val c2 = PureEffect.unsafeRunFuture(c1)
    val c3 = c2.map{ logger =>
      assert(logger.__all() == List("inside", "doSomething(r1)", "close r1"))
    }
    val r = Await.ready(c3, 1.second)
  }

}
