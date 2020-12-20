package cps

import org.junit.{Test,Ignore}
import org.junit.Assert._

import scala.quoted._
import scala.util.Success
import scala.util.Failure


class TestBS1SCustomShift:

  class ExampleSame[F[_]](m: CpsMonad[F]):

    var x = 0

    def simpleSame(f: Int=>Int): Int = f(x)

    def simpleSame_async(f: Int=> F[Int]): F[Int] = f(x)

    def reading(frs: Int, snd: =>Int)(f: Int => Int): Boolean = {
       x = frs
       frs < f(snd)
    }

    def reading_async(frs: Int, snd: ()=> F[Int])(f: Int => F[Int]): F[Boolean] = {
       x = frs
       m.flatMap(snd())(x => 
            m.map(f(x))( y => frs < y ))
    }


  class ExampleOther:

    var x = 0

    def simpleOther(f: Int=>Int): Int = f(x)

    def simpleOther_async[F[_]](m: CpsMonad[F], f: Int=> F[Int]): F[Int] = f(x)

    def reading(frs: Int, snd: =>Int)(f: Int => Int): Boolean = {
       x = frs
       frs < f(snd)
    }

    def reading_async[F[_]](m:CpsMonad[F], frs: Int, snd: ()=> F[Int])(f: Int => F[Int]): F[Boolean] = {
       x = frs
       m.flatMap(snd())(x => 
            m.map(f(x))( y => frs < y ))
    }




  @Test def testSimpleSame(): Unit = 
     //implicit val printCode = cps.macroFlags.PrintCode
     //implicit val debugLevel = cps.macroFlags.DebugLevel(20)
     val c = async[ComputationBound]{
        val sameApi = new ExampleSame[ComputationBound](summon[CpsMonad[ComputationBound]])
        sameApi.simpleSame(x => x + await(T1.cbi(1)))
     }
     assert(c.run() == Success(1))

  @Test def testSimpleOther(): Unit = 
     //implicit val printCode = cps.macroFlags.PrintCode
     //implicit val debugLevel = cps.macroFlags.DebugLevel(20)
     val c = async[ComputationBound]{
        val otherApi = new ExampleOther
        otherApi.simpleOther(x => x + await(T1.cbi(1)))
     }
     assert(c.run() == Success(1))


