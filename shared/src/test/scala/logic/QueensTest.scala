package logic

import cps.*
import logic.logict.{*,given}
import org.junit.{Ignore, Test}



class QueensTest {

  @Test
  def testLogicM() = {
    import QueensTest.*
    val r = queens[LogicM](8).observeN(2)
    // observer monad is identity monad here.
    println(s"QueensTest:r=$r")
    assert(r.size == 2)
    assert(QueensTest.isCorrect(r(0)))
    assert(QueensTest.isCorrect(r(1)))
  }

}

object QueensTest {

  case class Pos(x:Int, y:Int)

  def isBeat(p1:Pos, p2:Pos):Boolean =
    (p1.x == p2.x) || (p1.y == p2.y) || (p1.x - p1.y == p2.x - p2.y) || (p1.x + p1.y == p2.x + p2.y)

  def isFree(p:Pos, prefix:IndexedSeq[Pos]):Boolean =
    prefix.forall(pp => !isBeat(p, pp))

  def queens[M[+_]:CpsLogicMonad](n:Int, prefix:IndexedSeq[Pos]=IndexedSeq.empty): M[IndexedSeq[Pos]] = reify[M] {
    if (prefix.length >= n) then
      prefix
    else
      val nextPos = (1 to n).map(Pos(prefix.length+1,_)).filter(pos => isFree(pos, prefix))
      reflect(queens(n, prefix :+ reflect(all(nextPos))))
  }

  def isCorrect(queens:IndexedSeq[Pos]):Boolean = {
    queens.forall(p1 => queens.forall(p2 =>
       (p1 == p2) || !isBeat(p1,p2)
    ))
  }




}