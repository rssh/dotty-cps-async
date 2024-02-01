package cps.logic

import org.junit.{Test,Ignore}
import cps.*
import cps.monads.logic.{*,given}

class LazyListBasicTest {

  @Test
  def basicInterleave() = {
    val m1 = LazyList.from(List(1, 2, 3))
    val m2 = LazyList.from(List(4, 5, 6))
    assert(m1.toSeq == Seq(1, 2, 3))
    val m3 = m1 |+| m2
    assert(m3.toSeq == Seq(1, 2, 3, 4, 5, 6))
    val m4 = m2 |+| m1
    assert(m4.toLazyList.toSeq == Seq(4, 5, 6, 1, 2, 3))
    val m5 = m1 | m2
    //println(s"m5=${m5.toLazyList.toIndexedSeq}")
    assert(m5.toSeq == Seq(1, 4, 2, 5, 3, 6))
  }


}
