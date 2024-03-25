package cps.pe


import org.junit.{Test,Ignore}

import cps.*
import cps.testconfig.given
import cps.util.FutureCompleter


class TestUseResource {

  import scala.concurrent.ExecutionContext.Implicits.global

  def openResource(name:String, logger: PEToyLogger): Resource[PureEffect, String] = {
     Resource.Allocate(() => PureEffect.delay((name, () => logger.log(s"close $name"))))
  }

  @Test
  def testResourceInterop() = {
    val c = async[[X]=>>Resource[PureEffect,X]] {
              val logger = await(PEToyLogger.make())
              val r1 = await(openResource("r1", logger))
              await(logger.log("inside"))
              logger
    }
    val c1 = Resource.run(c)
    val c2 = PureEffect.unsafeRunFuture(c1)
    val c3 = c2.map{ logger =>
      assert(logger.__all() == List("inside", "close r1"))
    }
    FutureCompleter(c3)
  }

}
