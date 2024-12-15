package cps.pe

import org.junit.{Test,Ignore}
import cps.*

import cps.util.FutureCompleter
import cps.testconfig.given

import scala.concurrent.ExecutionContext.Implicits.global

class TestPEAsyncFinalizer {




  @Test
  def testLinearAsyncFinalizer() = {

    given cps.macros.flags.PrintCode.type = cps.macros.flags.PrintCode

    @volatile var x = 0
    @volatile var y = 0
    @volatile var nMainCalls = 0
    @volatile var nFinalizerCalls = 0
    val run = async[PureEffect] {
      try {
        x = 2
        await(PureEffect.delay(1))
        nMainCalls = nMainCalls + 1
      } finally {
        nFinalizerCalls = nFinalizerCalls + 1
        y = await(PureEffect.delay(2))
        if (x == 2) then
          x = 3
        else
          x = 1
      }
    }

    val c = run.map { _ =>
      assert(nFinalizerCalls == 1)
      assert(nMainCalls == 1)
      assert(x == 3)
      assert(y == 2)
    }

    val future = c.unsafeRunFuture()
    FutureCompleter(future)

  }


}
