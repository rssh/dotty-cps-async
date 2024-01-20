package cps.logic

import cps.*
import cps.monads.*
import cps.monads.logic.{*}
import org.junit.{Test,Ignore}

import scala.collection.immutable.TreeSet


class PrimesTest {

  @Test
  def testPrimes(): Unit = {
     val first10 = PrimesTest.primes.observeN(10)
     //println(s"first10 = $first10")
     assert(first10.toSeq == Seq(2,3,5,7,11,13,17,19,23,29))
  }

  @Test
  def testToLazyList() = {
      val first10 = PrimesTest.primes.toLazyList.take(10)
      //println(s"first10 = $first10")
      assert(first10.toSeq == Seq(2,3,5,7,11,13,17,19,23,29))
  }


}

object PrimesTest {

  def primes: LogicStream[Int] = {
    eratosphen(2, TreeSet.empty[Int])
  }


  def eratosphen(c:Int, knownPrimes: TreeSet[Int]): LogicStream[Int] = reify[LogicStream]{
        guard(
          knownPrimes.takeWhile(x => x*x <= c).forall(x => c % x != 0)
        )
        c
  } |+| eratosphen(c+1, knownPrimes + c)


}
