package cpstest

import cps.*
import cps.monads.{*, given}
import cps.CpsMonadConversion.given
import org.junit.Test

object TestSubtypeConversion {

  trait MA[T]

  abstract class MB[T]

  class MC extends MB[Int], MA[String]

  class TestCpsMonadMB extends CpsPureMonadInstanceContext[MB] {
    override def pure[T](t: T): MB[T] = ???

    override def map[A, B](fa: MB[A])(f: A => B): MB[B] = ???

    override def flatMap[A, B](fa: MB[A])(f: A => MB[B]): MB[B] = ???
  }

  given TestCpsMonadMB = new TestCpsMonadMB

  //given MCMBCpsMonadConversion: CpsMonadConversion[MC,MB] = new CpsMonadConversion[MC,MB] {
  //  override def apply[T](fa: MC[T]): MB[T] = ???
  //}

}

class TestSubtypeConversion {

  @Test
  def testSubtypeConversion(): Unit = {
    given cps.macros.flags.DebugLevel = cps.macros.flags.DebugLevel(20)
    /*
    import TestSubtypeConversion.{*, given}
    val f = async[MB] {
      val t = await(new MC)
      ()
    }
    
     */
  }

}


