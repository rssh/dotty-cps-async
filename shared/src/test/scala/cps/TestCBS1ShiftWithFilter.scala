package cps

import org.junit.{Test,Ignore}
import org.junit.Assert._

import scala.util.Success
import scala.util.Failure
import cps.testconfig.given


class TestCBS1ShiftWithFilter:

  @Test def testSimple1: Unit =
     //implicit val printCode = cps.macroFlags.PrintCode
     //implicit val printTree = cps.macroFlags.PrintTree
     //implicit val debugLevel = cps.macroFlags.DebugLevel(20)
     val c = async[ComputationBound]{
          for{ x <- 1 to 10 if (x%2) == await(T1.cbi(0)) 
             } yield (x)
     }
     val r = c.run().get
     assert(r(0) == 2 )

  @Test def testSimple1_map_shifted: Unit =
     //implicit val printCode = cps.macroFlags.PrintCode
     //implicit val printTree = cps.macroFlags.PrintTree
     //implicit val debugLevel = cps.macroFlags.DebugLevel(20)
     val c = async[ComputationBound]{
          for{ x <- 1 to 10 if (x%2) == await(T1.cbi(0)) 
             } yield (await(T1.cbi(x)))
     }
     val r = c.run().get
     assert(r(0) == 2 )

  @Test def testSimple2l: Unit =
     //implicit val printCode = cps.macroFlags.PrintCode
     //implicit val printTree = cps.macroFlags.PrintTree
     //implicit val debugLevel = cps.macroFlags.DebugLevel(20)
     val c = async[ComputationBound]{
          for{ x <- 1 to 10 if (x%2) == await(T1.cbi(0)) 
               y <- 20 to 30 if (y%2) == await(T1.cbi(1)) 
          } yield x+y
     }
     val r = c.run().get
     assert(r(0) == 23 )

  @Test def testSimple2l1_p1: Unit =
     //implicit val printCode = cps.macroFlags.PrintCode
     //implicit val printTree = cps.macroFlags.PrintTree
     //implicit val debugLevel = cps.macroFlags.DebugLevel(20)
     val c = async[ComputationBound]{
          (1 to 10).withFilter( x => (x%2 == await(T1.cbi(0))) )
                   .flatMap( x =>
                      (20 to 30).withFilter( y => y % 2 == await(T1.cbi(1)) )
                                .map(y => x+y)
                   )
     }
     val r = c.run().get
     assert(r(0) == 23 )

  //*
  // same crash unsugared - simplifications
  @Test def testSimple2l1_p3: Unit =
     //implicit val printCode = cps.macroFlags.PrintCode
     //implicit val printTree = cps.macroFlags.PrintTree
     //implicit val debugLevel = cps.macroFlags.DebugLevel(20)
     val c = async[ComputationBound]{
          (1 to 10).withFilter( x => x == await(T1.cbi(1)) )
                   .flatMap( x =>
                      (1 to 10).withFilter( y => y == await(T1.cbi(1)) )
                        .map(_ => x)
                   )
     }
     val r = c.run().get
     assert(r(0) == 1 )
  //*/

  //*
  // no-crash when adding await
  @Test def testSimple2l1_p2: Unit =
     //implicit val printCode = cps.macroFlags.PrintCode
     //implicit val printTree = cps.macroFlags.PrintTree
     //implicit val debugLevel = cps.macroFlags.DebugLevel(20)
     val c = async[ComputationBound]{
          (1 to 10).withFilter( x => (x%2 == await(T1.cbi(0))) )
                   .flatMap( x =>
                      (20 to 30).withFilter( y => y % 2 == await(T1.cbi(1)) )
                                .map(y => x+ await(T1.cbi(y)))
                   )
     }
     val r = c.run().get
     assert(r(0) == 23 )
     //*/

  // fixed by https://github.com/lampepfl/dotty/pull/10142
  @Test def testSimple2l_p1_2: Unit =
     //implicit val printCode = cps.macroFlags.PrintCode
     //implicit val printTree = cps.macroFlags.PrintTree
     //implicit val debugLevel = cps.macroFlags.DebugLevel(20)
     import cps.runtime._
     val c = async[ComputationBound]{
          (1 to 100).map( zDebug =>
                      await(  
                        T1.cbi(1).map(a => zDebug + a)                     
                      )
                   )
     }
     val r = c.run().get


  @Test def testSimple2l_p3: Unit =
     //implicit val printCode = cps.macroFlags.PrintCode
     //implicit val printTree = cps.macroFlags.PrintTree
     //implicit val debugLevel = cps.macroFlags.DebugLevel(20)
     import cps.runtime._
     val c = async[ComputationBound]{
          await(RangeAsyncShift[Range.Inclusive]().withFilter((1 to 10), ComputationBoundAsyncMonad)( x => 
                     T1.cbi(0).map(a => (x%2 == a) ))
                   .flatMap_async( x =>
                       RangeAsyncShift[Range.Inclusive]().withFilter(20 to 30, ComputationBoundAsyncMonad)( 
                                  y => T1.cbi(1).map( a => a == y%2 )  )
                                .map(y => x+y)
                   ))
     }
     val r = c.run().get
     assert(r(0) == 23 )


  @Test def testSimple2l2: Unit =
     //implicit val printCode = cps.macroFlags.PrintCode
     //implicit val printTree = cps.macroFlags.PrintTree
     //implicit val debugLevel = cps.macroFlags.DebugLevel(20)
     val c = async[ComputationBound]{
          for{ x <- 1 to 10 if (x%2) == await(T1.cbi(0)) 
               y <- 20 to 30 
          } yield (x,y)
     }
     val r = c.run().get
     assert(r(0) == (2,20) )


  @Test def testSimple2l2sync: Unit =
     //implicit val printCode = cps.macroFlags.PrintCode
     //implicit val printTree = cps.macroFlags.PrintTree
     //implicit val debugLevel = cps.macroFlags.DebugLevel(20)
     val c = async[ComputationBound]{
          for{ x <- 1 to 10 if (x%2) == await(T1.cbi(0)) 
               y <- 20 to 30 if y%2 == 1
          } yield (x,y)
     }
     val r = c.run().get
     assert(r(0) == (2,21) )


