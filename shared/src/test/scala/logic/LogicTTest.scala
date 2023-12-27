package logic

import scala.concurrent.duration.*
import scala.util.*
import cps.*
import logict.*
import org.junit.Test

class LogicTTest {

  import LogicTTest.*

  @Test
  def testNatCB():Unit = {

     val cbNat = nats[[A] =>> LogicT[ComputationBound,A]]

     val cb4 = cbNat.observeN(4)
     assert(cb4.run(1.second) == Success(Seq(1,2,3,4)))

     val cbOdd = odds[[A] =>> LogicT[ComputationBound,A]]



  }


}

object LogicTTest {

  def nats[M[_]](using m: CpsLogicMonad[M]): M[Int] =
    m.pure(1) |+| reify[M] { 1 + reflect(nats) }

  def odds[M[_]](using m: CpsLogicMonad[M]): M[Int] =
    m.pure(1) |+| reify[M] { 2 + reflect(odds) }




}
