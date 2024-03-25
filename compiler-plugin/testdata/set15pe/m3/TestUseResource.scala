package cps.pe

import scala.annotation.experimental
import scala.concurrent.*
import scala.concurrent.duration.*
import org.junit.{Test,Ignore}

import cps.*
//  import cps.testconfig.given


@experimental
class TestUseResource {

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

  @Test
  def testResourceInterop() = {
    val c = async[[X]=>>Resource[PureEffect,X]] {
              val logger = PEToyLogger.make()
              val r1 = await(openResource("r1", logger))
              logger.log("inside")
              logger
    }
    val c1 = Resource.run(c)
    val c2 = PureEffect.unsafeRunFuture(c1)
    val c3 = c2.map{ logger =>
      assert(logger.__all() == List("inside", "close r1"))
    }
    val r = Await.ready(c3, 1.second)
  }

}
