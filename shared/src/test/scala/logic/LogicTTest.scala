package logic

import scala.concurrent.duration.*
import scala.util.*
import cps.*
import logic.logict.LogicStreamT.MPlusSeq
import logict.*
import org.junit.Test

class LogicTTest {

  import LogicTTest.*

  type LogicSKFKCB[A] = LogicTSKFK[ComputationBound,A]

  def runNatCB[M[_]]()(using m:CpsLogicMonad[M] { type Observer[A] = ComputationBound[A] } ): Unit = {
    
    val cbNat = nats[M]

    val cb4 = cbNat.observeN(4)
    assert(cb4.run(1.second) == Success(Seq(1,2,3,4)))

    //val cbStream = cbNat

    val cbOdds = odds[M]
    val odds4 = cbOdds.observeN(4)
    assert(odds4.run(1.second) == Success(Seq(1,3,5,7)))

    val cbOdds2unfair = cbOdds  || m.pure(2)

    val cbOdds2fair = cbOdds | m.pure(2)

    val odds2unfair4 = cbOdds2unfair.observeN(4)
    assert(odds2unfair4.run(1.second) == Success(Seq(1,3,5,7)))

    val odds2fair4 = cbOdds2fair.observeN(4)
    val odds4fair2result = odds2fair4.run(2.second).get


    assert(odds4fair2result.contains(1))
    assert(odds4fair2result.contains(2))
    assert(odds4fair2result.contains(3))
    assert(odds4fair2result.contains(5))
    assert(odds4fair2result.size == 4)
    
  }

  @Test
  def runLogicStreamOdds2Fair(): Unit = {
    val m = summon[CpsLogicMonad[[A]=>>LogicStreamT[ComputationBound,A]]]
    val cbNat = nats[[T]=>>LogicStreamT[ComputationBound,T]]
    val cbOdds = odds[[T]=>>LogicStreamT[ComputationBound,T]]

    cbOdds.observeN(4).run(1.second) match
      case Success(v) =>
        println(s"cbOddsForLogStream=$v")
      case Failure(ex) =>
        ex.printStackTrace()

    val cbOdds2fair = cbOdds | m.pure(2)

    val odds2fair4 = cbOdds2fair.observeN(5)
    val odds4fair2result = odds2fair4.run(2.second).get


    assert(odds4fair2result.contains(5))

  }

  @Test
  def testNatCBSKFK():Unit = {
    runNatCB[LogicSKFKCB]()
  }

  @Test
  def testNatCBSeq(): Unit = {
    runNatCB[[A]=>>LazyLogicSeqT[ComputationBound,A]]()
  }

  @Test
  def testNatCBLogicStream(): Unit = {
    runNatCB[[A]=>>LogicStreamT[ComputationBound,A]]()
  }

  @Test
  def testNatCbOnce(): Unit = {
    val m = summon[CpsLogicMonad[LogicSKFKCB]]
    val myOnce = m.once(m.pure(1))
    val myOnce4 = myOnce.observeN(4)
    assert(myOnce4.run(1.second) == Success(Seq(1)))
  }

  @Test
  def testNatCbFlatMap0(): Unit = {
    import cps.syntax.*
    val m = summon[CpsLogicMonad[LogicSKFKCB]]
    val cbOdds = odds[LogicSKFKCB]
    val cnOddsFlatMapZero = cbOdds.flatMap { x => m.mzero[Int] }
    //  infinitie loop.
    //  we see this, but can't detect in compile time.
    //val res4 = cnOddsFlatMapZero.observeN(4)
    //assert(res4.run(1.second) == Success(Seq(2)))
  }

  @Test
  def testOnceNat(): Unit = {
    val m = summon[CpsLogicMonad[LogicSKFKCB]]
    val myOnce = m.once(nats)
    val myOnce4 = myOnce.observeN(4)
    assert(myOnce4.run(1.second) == Success(Seq(1)))
  }


  @Test
  def testNatCbFlatMap1(): Unit = {
    import cps.syntax.*
    val m = summon[CpsLogicMonad[LogicSKFKCB]]
    val cbOddsOrTwo = oddsOrTwo[LogicSKFKCB]
    val cnOddsOrTwoFlatMap = cbOddsOrTwo.flatMap{ x =>
      if (x % 2 == 0) then m.pure(x) else m.mzero[Int]
    }
    val res4 = cnOddsOrTwoFlatMap.observeOne
    assert(res4.run(1.second) == Success(Some(2)))
  }



  /*
  @Test
  def testNatCbOddsOrTwo(): Unit = {

    val cbOddsOrTwo = oddsOrTwo[LogicSKFKCB]
    val oddsOrTwo4 = cbOddsOrTwo.observeN(4)
    println(s"oddsOrTwo4 = ${oddsOrTwo4.run(1.second)}")
    oddsOrTwo4.run(1.second) match
      case Success(v) =>
        println(s"v=$v")
      case Failure(ex) =>
        ex.printStackTrace()

  }

   */





}

object LogicTTest {

  def nats[M[_]](using m: CpsLogicMonad[M]): M[Int] =
    m.pure(1) |+| reify[M] { 1 + reflect(nats) }

  def odds[M[_]](using m: CpsLogicMonad[M]): M[Int] =
    m.pure(1) |+| reify[M] { 2 + reflect(odds) }


  def oddsOrTwoUnfair[M[_]](using m:CpsLogicMonad[M]) = odds |+| m.pure(2)

  def oddsOrTwoFair[M[_]](using m: CpsLogicMonad[M]) = odds | m.pure(2)

  def oddsOrTwo[M[_]](using m: CpsLogicMonad[M]) = {
    import cps.syntax.*
    for {
      x <- oddsOrTwoFair
      y <- if (x % 2 == 0) then m.once(m.pure(x)) else m.mzero[Int]
    } yield {
      println(s"y=$y")
      y
    }
  }

  def odds5down[M[_]](using m:CpsLogicMonad[M]) =
     m.pure(5) |+| m.mzero |+| m.mzero |+| m.pure(3) |+| m.pure(1)


  def yieldWords[M[_]:CpsLogicMonad](words: String*): M[String] =
    yieldWordsList(words.toList)

  def yieldWordsList[M[_]](words: List[String])(using m: CpsLogicMonad[M]): M[String] =
    if (words.isEmpty) then m.mzero[String]
    else m.pure(words.head) |+| reify[M] { reflect(yieldWordsList(words.tail)) }

}
