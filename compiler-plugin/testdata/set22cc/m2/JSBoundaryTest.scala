package jsasync

import cps.*
import gears.async.{JSAsync, JSAsyncSupport}
import scala.concurrent.*
import scala.concurrent.duration.*
import scala.concurrent.ExecutionContext.Implicits.global

import gears.async.given
import cps.plugin.annotation.CpsDebugLevel


@CpsDebugLevel(20)
class JSBoundaryTest {


  @CpsDebugLevel(20)
  def simpleFlowWithSuspension = {
    println("simple flow with suspension:start")
    val r = async[JSAsync] {
      println("in async")
      var q = 1
      println("before boundary")
      JSAsyncSupport.boundary{
        println("in boundary")
        q = 2
        println("before suspend")
        val k = JSAsyncSupport.suspend[Int,Int]{ s =>
          println("before resume")
          val r = s.resume(3)+1
          println("after resume")
          r
        }
        println("after suspend")
        if (k == 3) then
          q = 10
          println("Ok")
        q+k
      }
    }
    r.toFuture.map{ x =>
      println(s"result = $x")
      assert(x == 14)
    }
  }


  /*
  def simpleFloWithBoundaryButWithoutSuspension = {
    var r = async[JSAsync] {
      var q = 1
      JSAsyncSupport.boundary{
        q = 2
        val k = 3
        if (k == 3) then
          q = 10
        q+k
      }
    }
    r.toFuture.map{ x =>
      assert(x == 13)
    }
  }

   */

}
