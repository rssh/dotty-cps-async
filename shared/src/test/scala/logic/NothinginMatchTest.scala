package logic

import cps.*
import cps.monads.{*,given}
import logic.logict.{*,given}

import org.junit.{Test,Ignore}

extension [M[_]](mc:CpsLogicMonadContext[M])

  transparent inline def noChoices[A]: A = {
    reflect(mc.monad.mzero[A])(using mc, CpsMonadConversion.identityConversion[M])
  }


transparent inline def noChoices[M[_],A](using mc: CpsLogicMonadContext[M]): A = {
  reflect(mc.monad.mzero[A])
}


// syntax not working yet.
//transparent inline def noChoices2[M](using mc: CpsLogicMonadContext[M])[A]: A = {
//  reflect(mc.monad.mzero[A])
//}



class NothinginMatchTest {

  def ifNoNull[M[_]:CpsLogicMonad](x:Int):M[Int] = reify[M] {
    if (x == 0) {
      noChoices
    } else {
      x
    }
  }

  def matchNoZero[M[_]:CpsLogicMonad](x:Int):M[Int] = reify[M] {
    x match
      case 0 => summon[CpsLogicMonadContext[M]].noChoices[Int]
      case _ => x
  }


  @Test
  def testIfNoChoices(): Unit = {
     val es = ifNoNull[LogicStream](0)
     assert(es.observeOne.isEmpty)
  }

  @Test
  def testMatchNoChoices(): Unit = {
    val es = matchNoZero[LogicStream](0)
    assert(es.observeOne.isEmpty)
  }

}
