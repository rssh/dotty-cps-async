package futureScope.examples

import scala.concurrent.*
import scala.concurrent.duration.*
import scala.util.*
import futureScope.*

import cps.*
import cps.monads.{*,given}
import cps.testconfig.given
import cps.util.FutureCompleter


import org.junit.{Test,Ignore}
import org.junit.Assert.*


class TestExceptionPropagation {

  import scala.concurrent.ExecutionContext.Implicits.global

  @Test def testExceptionPropagation() = {
    val f0 = async[Future].in(Scope){

      FutureScope.spawn[Unit]{
        // give time for second process to spawn
        //println("testExceptionPropagation: before await")
        await(FutureScope.spawnDelay(10.milliseconds))
        throw new RuntimeException("exception:p1")
        () // TODO: problem ij cps-transform..
      }
      val p2 = FutureScope.spawn{
        val p = Promise[Int]()
        FutureScope.timedAwait(p.future, 1.minute)  
      }

      await(p2)
    }
    val f = f0.transform{
      case Failure(ex) => 
           //println(s"message:${ex.getMessage()}")
           //ex.printStackTrace()
           if (!ex.getMessage().nn.contains("exception:p1")) then
              println(s"here will be asset false, message:${ex.getMessage()}")
              ex.printStackTrace()
           assert(ex.getMessage().nn.contains("exception:p1"))
           Success(true)
      case Success(_) =>
           Failure(new RuntimeException("success expected"));
    }
    FutureCompleter(f)
  }

  @Test def testDisanbleEscalation() = {
    val f = async[Future].in(Scope){

      FutureScope.spawn{
        Try{
          throw new RuntimeException("exception:p1")
        }
      }
      val p2 = FutureScope.spawn{
        val p = Promise[Int]()
        await(FutureScope.spawnDelay(100.milliseconds))
        p.success(1)
        await(p.future)  
      }

      val r = await(p2)
      assert(r == 1)
    }
    FutureCompleter(f)
  }

}
