package cpstest

import org.junit.{Test,Ignore}
import org.junit.Assert.*

import scala.concurrent.*
import scala.concurrent.duration.*
import scala.concurrent.ExecutionContext.Implicits.global

import cps.*
import cps.monads.{*,given}
import cps.stream.*

import cps.macros.flags.*

given UseCompilerPlugin.type = UseCompilerPlugin



class TestAsyncListStress:


  @Test @Ignore  def testMergeKNonEmptyTimedLists() = {


      val f = async[Future] {

        val stream = asyncStream[AsyncList[Future,Int]] { out =>
            val delay = 10.milliseconds
            await(FutureSleep(delay))
            out.emit(1)
        }

      }

      println(s"TestAsycListMetgeStress: waiting for result")
      Await.result(f, 1.minute)

  }


  def FutureSleep(d: Duration): Future[Unit] =
    ???