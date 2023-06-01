package cpstest


import scala.util.*
import cps.*
import cps.monads.{*, given}
import org.junit.{Ignore, Test}

import scala.util.boundary.break
import scala.util.control.NonFatal

class TestCBBoundary {

  @Test def testBoundary1t(): Unit = {
    //implicit val printCode = cps.macroFlags.PrintCode
    //implicit val debugLevel = cps.macros.flags.DebugLevel(5)
    val condition = true
    val c = async[ComputationBound] {
      boundary {
        if (condition) then
          break(1)
        await(T1.cbi(2))
        3
      }
    }
    val r = c.run()
    assert(r.isSuccess)
    r match
      case Success(v) => assert(v == 1)
      case Failure(ex) => throw ex
  }


  @Test def testBoundary1f(): Unit = {
    //implicit val printCode = cps.macros.flags.PrintCode
    //implicit val debugLevel = cps.macros.flags.DebugLevel(5)
    val condition = false
    val c = async[ComputationBound] {
      boundary {
        if (condition) then
          break(1)
        await(T1.cbi(2))
        3
      }
    }
    val r = c.run()
    assert(r.isSuccess)
    r match
      case Success(v) => assert(v == 3)
      case Failure(ex) => throw ex
  }

  @Test def testSync(): Unit = {
    //implicit val printCode = cps.macros.flags.PrintCode
    //implicit val debugLevel = cps.macros.flags.DebugLevel(10)
    val c = async[ComputationBound] {
      boundary {
        if (true) then
          break(1)
        3
      }
    }
    val r = c.run()
    assert(r.isSuccess)
    r match
      case Success(v) => assert(v == 1)
      case Failure(ex) => throw ex
  }

  @Test def testSyncNoBreak(): Unit = {
    //implicit val printCode = cps.macros.flags.PrintCode
    //implicit val debugLevel = cps.macros.flags.DebugLevel(10)
    val c = async[ComputationBound] {
      boundary {
        3
      }
    }
    val r = c.run()
    assert(r.isSuccess)
    r match
      case Success(v) => assert(v == 3)
      case Failure(ex) => throw ex
  }


  @Test def testBoundary2ft(): Unit = {
    //implicit val printCode = cps.macros.flags.PrintCode
    //implicit val printTree = cps.macros.flags.PrintTree
    //implicit val debugLevel = cps.macros.flags.DebugLevel(5)
    val c1 = false
    val c2 = true
    val c = async[ComputationBound] {
      boundary {
        if (c1) then
          break(1)
        await(T1.cbi(2))
        if (c2) then
          break(2)
        3
      }
    }
    val r = c.run()
    //println(s"c == $c")
    assert(r.isSuccess)
    r match
      case Success(v) => assert(v == 2)
      case Failure(ex) => throw ex
  }

  @Test def testBoundary3ft(): Unit = {
    //implicit val printCode = cps.macros.flags.PrintCode
    //implicit val printTree = cps.macros.flags.PrintTree
    //implicit val debugLevel = cps.macros.flags.DebugLevel(5)
    val c1 = false
    val c2 = false
    val c = async[ComputationBound] {
      boundary {
        if (c1) then
          break(1)
        await(T1.cbi(2))
        if (c2) then
          break(2)
        3
      }
    }
    val r = c.run()
    //println(s"c == $c")
    assert(r.isSuccess)
    r match
      case Success(v) => assert(v == 3)
      case Failure(ex) => throw ex
  }

  @Test def testBoundaryFromTry(): Unit = {
    //implicit val printCode = cps.macros.flags.PrintCode
    //implicit val printTree = cps.macros.flags.PrintTree
    //implicit val debugLevel = cps.macros.flags.DebugLevel(15)
    val c = async[ComputationBound] {
      boundary {
        try
          await(T1.cbi(2))
          break(2)
        catch
          case NonFatal(ex) =>
            8
      }
    }
    val r = c.run()
    //println(s"r == $r")
    //  here situation will be differ than in returning, righat answer is 8
    assert(r.isSuccess)
    r match
      case Success(v) => assert(v == 8)
      case Failure(ex) => throw ex
  }

  transparent inline def earlyBreak[T](t: T)(using boundary.Label[T]): Nothing =
    break(t)

  @Test def inlineSubstForBoundary(): Unit = {
    val condition1 = true
    val c = async[ComputationBound] {
      boundary {
        await(T1.cbi(2))
        if (condition1) {
          earlyBreak(1)
        }
        10
      }
    }
    val r = c.run()
    r match
      case Success(v) => assert(v == 1)
      case Failure(ex) => throw ex;
  }


}
