package cps.logic

import org.junit.{Test,Ignore}

import cps.*
import cps.monads.logic.*

class DefaultLogicMonadBasicTest {

  @Test
  def basicInterleaveFromCollection() = {
    val m1 = LogicStream.fromCollection(List(1,2,3))
    val m2 = LogicStream.fromCollection(List(4,5,6))
    assert(m1.toLazyList.toSeq == Seq(1,2,3))
    val m3 = m1 |+| m2
    assert(m3.toLazyList.toSeq == Seq(1,2,3,4,5,6))
    val m4 = m2 |+| m1
    assert(m4.toLazyList.toSeq == Seq(4,5,6,1,2,3))
    val m5 = m1 | m2
    //println(s"m5=${m5.toLazyList.toIndexedSeq}")
    assert(m5.toLazyList.toSeq == Seq(1,4,2,5,3,6))
  }

  @Test
  def basicFairFlatMap() = {
    val m1 = LogicStream.fromCollection(List(1, 2))
    val m2 = m1 &>> { x =>
      if (x == 2) {
        LogicStream.fromCollection(List(4, 5, 6))
      } else {
        LogicStream.fromCollection(List(7, 8, 9))
      }
    }
    assert(m2.toLazyList.toSeq == Seq(7,4,8,5,9,6))
  }

  @Test
  def testBaiscOnce(): Unit = {
    val m1 = LogicStream.fromCollection(List(1, 2))
    val m2 = once(m1)
    assert(m2.toLazyList.toSeq == Seq(1))
  }
  
  @Test
  def testEmpty() = {
    val m1 = LogicStream.fromCollection(List.empty[Int])
    val m2 = LogicStream.fromCollection(List(1, 2))
    val m3 = m1 |+| m2
    assert(m1.toLazyList.toSeq == Seq.empty)
    assert(m2.toLazyList.toSeq == Seq(1,2))
    assert(m3.toLazyList.toSeq == Seq(1,2))
  }



}


