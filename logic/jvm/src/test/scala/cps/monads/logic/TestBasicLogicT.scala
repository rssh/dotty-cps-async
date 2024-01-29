package cps.monads.logic

import scala.concurrent.*
import scala.concurrent.duration.*
import scala.concurrent.ExecutionContext.Implicits.global
import org.junit.{Test,Ignore}

import cps.*
import cps.monads.{*,given}
import cps.monads.logic.{*,given}

class TestBasicLogicT {


  @Test
  def testOneMplus(): Unit = {
       val x = LogicStreamT.fpure(Future successful 1)
       val y = LogicStreamT.fpure(Future successful 2)
       val z = x |+| y
       val zl = z.toAsyncList
       val fr = zl.next.flatMap{
          case Some((l, r1)) =>
            assert(l == 1)
            r1.next.flatMap{
              case Some((l,r2)) =>
                assert(l == 2)
                r2.next.flatMap{
                  case None => Future successful true
                  case _ => Future successful false
                }
              case None => Future successful false
            }

       }
       Await.result(fr, 1.second)
  }

  @Test
  def testParOr(): Unit = {
    //TODO
  } 
  
}
