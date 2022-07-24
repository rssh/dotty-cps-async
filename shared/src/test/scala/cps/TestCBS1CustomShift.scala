package cps

import org.junit.{Test,Ignore}
import org.junit.Assert._

import scala.quoted._
import scala.util.Success
import scala.util.Failure

import cps.testconfig.given

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

    def doOverload(a:Int, f: (Int,Int)=>Int): Int = f(a,x)

    def doOverload(a: String, f: String=>String): String = f(x.toString)

    def doOverload(v: Int, f: Int=>Int): Int = f(v)

    def doOverload_async(a:Int, f: (Int,Int)=>F[Int]): F[Int] = f(a,x)

    def doOverload_async(a:String, f: String=>F[String]): F[String] = f(x.toString)

    def doOverload_async(v: Int, f: Int=>F[Int]): F[Int] = f(v)


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


    def doOverload(f: Int=>Int): Int = f(x)

    def doOverload_async[F[_]](m: CpsMonad[F], f: Int=>Int): Int = f(x)

    def doOverload(a:Int, f: (Int,Int)=>Int): Int = f(a,x)

    def doOverload_async[F[_]](m: CpsMonad[F], a:Int, f: (Int,Int)=>F[Int]): F[Int] = f(a,x)

    def doOverload(a:String, f: String=>String): String = f(a)

    def doOverload_async[F[_]](a:String, f: String=>F[String]): F[String] = f(x.toString)


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


  @Test def testReadingSame(): Unit = 
     val c = async[ComputationBound]{
        val sameApi = new ExampleSame[ComputationBound](summon[CpsMonad[ComputationBound]])
        sameApi.reading(1,2)( x => x + await(T1.cbi(1)) )
     }
     assert(c.run() == Success(true))


  @Test def testOverload1Same(): Unit = 
     val c = async[ComputationBound]{
        val sameApi = new ExampleSame[ComputationBound](summon[CpsMonad[ComputationBound]])
        sameApi.doOverload(1,(x,y) => x + await(T1.cbi(y)))
     }
     assert(c.run() == Success(1))


  @Test def testOverload1Other(): Unit = 
     val c = async[ComputationBound]{
        val otherApi = new ExampleOther
        otherApi.doOverload(2,(x,y) => x + await(T1.cbi(y)))
     }
     assert(c.run() == Success(2))


