package cps.cef

import cps.testconfig.given
import cps.*

import scala.util.*

import org.junit.{Test,Ignore}
import org.junit.Assert._

import cps.automaticColoring.given

class TestCef98 {

    given CpsMonad[Option] with CpsPureMonadInstanceContext[Option]  with {
      def pure[T](t:T):Option[T] = Some(t)
      def map[A,B](fa:Option[A])(f: A=>B):Option[B] = fa.map(f)
      def flatMap[A,B](fa:Option[A])(f: A=>Option[B]):Option[B] = fa.flatMap(f)
    }

    given CpsMonadMemoization.Default[Option] with {}

    given CpsMonadMemoization.Inplace[ComputationBound] with {
      def apply[T](ft:ComputationBound[T]): ComputationBound[T] =
        ComputationBound.spawn(ft)
    }

    @Test
    def testNestedAwaits() = {

      //implicit val printCode = cps.macros.flags.PrintCode

      val optionV: Option[ComputationBound[Int]] = Some(Done(1))
      val c = async[ComputationBound] {
        await(async[Option](await(optionV)).get)
      }
      val r = c.run()
      assert(r == Success(1))
    }

}

