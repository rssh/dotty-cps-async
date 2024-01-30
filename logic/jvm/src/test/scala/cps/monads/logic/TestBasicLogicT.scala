package cps.monads.logic

import scala.concurrent.*
import scala.concurrent.duration.*
import scala.concurrent.ExecutionContext.Implicits.global
import scala.collection.immutable.TreeSet
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

  def primes(n:Int=2, knownPrimes: TreeSet[Int]=TreeSet.empty): LogicStreamT[Future,Int] = reify[[T]=>>LogicStreamT[Future,T]]{
     guard{
       knownPrimes.takeWhile(p => p*p <= n).forall(p => n % p != 0)
     }
     //Future just to use async interface.
     n
  } |+| primes(n+1, knownPrimes + n)

  def fibs(n1:Int=0, n2:Int=1): LogicStreamT[Future,Int] = {
    LogicStreamT.mpure[Future,Int](n1) |+| fibs(n2, n1+n2)
  }

  @Test
  def testParOr(): Unit = {
        val N = 100
        val x = primes()
        val y = fibs()
        val z = x parOr y
        val zList = z.toAsyncList
        val fFirstN = zList.take(N)
        val firstN = Await.result(fFirstN, 1.second)
        //println(s"firstN = ${firstN.toSeq}")
        assert(firstN.size == N)
        assert(firstN.find(_ == 8).isDefined)
        assert(firstN.find(_ == 7).isDefined)
  }

  @Test
  def testParOrOne(): Unit = {
    val N = 100
    val x = primes()
    val y = LogicStreamT.mpure[Future,Int](8)
    val z = x parOr y
    val zList = z.toAsyncList
    val fFirstN = zList.take(N)
    val firstN = Await.result(fFirstN, 1.second)
    //println(s"firstN = ${firstN.toSeq}")
    assert(firstN.size == N)
    assert(firstN.find(_ == 8).isDefined)
    assert(firstN.find(_ == 7).isDefined)
  }
  

}
