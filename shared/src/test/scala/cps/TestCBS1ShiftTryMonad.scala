package cps

import org.junit.{Test,Ignore}
import org.junit.Assert._

import scala.util._
import cps.testconfig.given


class TestCBS1ShiftTryMonad:

  @Test def testMapTry(): Unit = 
     //implicit val printCode = cps.macroFlags.PrintCode
     //implicit val debugLevel = cps.macroFlags.DebugLevel(20)
     val c = async[ComputationBound]{
            val q = summon[CpsTryMonad[ComputationBound]].mapTry(T1.cbi(2)){
               case Success(x) => x + await(T1.cbi(3))
               case Failure(ex) => 0
            }
            await(q)
     }
     val r = c.run()
     assert(c.run() == Success(5))

  @Test def testMapTry1(): Unit = 
     //implicit val printCode = cps.macroFlags.PrintCode
     //implicit val debugLevel = cps.macroFlags.DebugLevel(20)
     val c: ComputationBound[Int] = async[ComputationBound]{ 
            val q = summon[CpsTryMonad[ComputationBound]].mapTry(T1.cbi(2)){ v =>
              v match
                 case Success(x) => x + await(T1.cbi(3))
                 case Failure(ex) => 0
            }
            await(q)
     }
     val r = c.run()
     assert(c.run() == Success(5))

  @Test def testFlatMapTry(): Unit = 
     //implicit val printCode = cps.macros.flags.PrintCode
     //implicit val debugLevel = cps.macros.flags.DebugLevel(20)
     val c = async[ComputationBound]{
          val q = summon[CpsTryMonad[ComputationBound]].flatMapTry(T1.cbi(2)){ v =>
              v match
                case Success(x) => T1.cbi(x + await(T1.cbi(3)))
                case Failure(ex) => T1.cbi(0)
          }
          await(q)
     }
     val v = c.run()
     //println(s"TestFlatMapTry: v=$v")
     assert(c.run() == Success(5))


  @Test def testRestore(): Unit = 
     val c = async[ComputationBound]{
         val fa = T1.cbs("fa")
         val q = summon[CpsTryMonad[ComputationBound]].restore(fa)(ex =>
              T1.cbs(ex.toString + await(T1.cbs("!!!"))) 
         )
         await(q)
     }
     assert(c.run() == Success("fa"))

  @Test def testRestoreNeg(): Unit = 
     val c = async[ComputationBound]{
         val fa  = Error[String](new RuntimeException("fa"))
         val q = summon[CpsTryMonad[ComputationBound]].restore(fa)(ex =>
              T1.cbs(ex.toString + await(T1.cbs("!!!"))) 
         )
         await(q)
     }
     val Success(v) = c.run()
     assert(v.endsWith("!!!"))

  @Test def testWithAction(): Unit = 
     val c = async[ComputationBound]{
         var x = 0 
         val fa = T1.cbs("fa")
         val q = summon[CpsTryMonad[ComputationBound]].withAction(fa)( {x = await(T1.cbi(1))} )
         await(q)
         x
     }
     assert(c.run() == Success(1))



