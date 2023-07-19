package cps


import org.junit.{Test,Ignore}
import org.junit.Assert._

import scala.util.*


import cps.*
import cps.testconfig.given


class TestCBInfix {


  @Test def testBitwiseAnd_0_0(): Unit = {
    //implicit val printCode = cps.macroFlags.PrintCode
    //implicit val debugLevel = cps.macros.flags.DebugLevel(5)
    var x = 0
    var y = 0
    var xCalled = false
    var yCalled = false
    val c = async[ComputationBound] {
      {
        xCalled = true; x
      } & {
        yCalled = true; y
      }
    }
    val r = c.run()
    assert(r.isSuccess)
    r match
      case Success(v) =>
        assert(xCalled == true)
        assert(yCalled == true)
      case Failure(ex) => throw ex
  }

  @Test def testBitwiseAnd_0_1(): Unit = {
    //implicit val printCode = cps.macroFlags.PrintCode
    //implicit val debugLevel = cps.macros.flags.DebugLevel(5)
    var x = 0
    var y = 1
    var xCalled = false
    var yCalled = false
    val c = async[ComputationBound] {
      {
        xCalled = true; x
      } & await(T1.cbt {
        yCalled = true; y
      })
    }
    val r = c.run()
    assert(r.isSuccess)
    r match
      case Success(v) =>
        assert(v == 0)
        assert(xCalled == true)
        assert(yCalled == true)
      case Failure(ex) => throw ex
  }

  @Test def testBitwiseAnd_1_0(): Unit = {
    //implicit val printCode = cps.macroFlags.PrintCode
    //implicit val debugLevel = cps.macros.flags.DebugLevel(5)
    var x = 1
    var y = 0
    var xCalled = false
    var yCalled = false
    val c = async[ComputationBound] {
      await(T1.cbt {
        xCalled = true; x
      }) & {
        yCalled = true; y
      }
    }
    val r = c.run()
    assert(r.isSuccess)
    r match
      case Success(v) =>
        assert(v == 0)
        assert(xCalled == true)
        assert(yCalled == true)
      case Failure(ex) => throw ex
  }

  @Test def testBitwiseAnd_1_1(): Unit = {
    //implicit val printCode = cps.macroFlags.PrintCode
    //implicit val debugLevel = cps.macros.flags.DebugLevel(5)
    val x = 1
    val y = 1
    var xCalled = false
    var yCalled = false
    val c = async[ComputationBound] {
      await(T1.cbt {
        xCalled = true; x
      }) & await(T1.cbt {
        yCalled = true; y
      })
    }
    val r = c.run()
    assert(r.isSuccess)
    r match
      case Success(v) =>
        assert(v == 1)
        assert(xCalled == true)
        assert(yCalled == true)
      case Failure(ex) => throw ex
  }

  @Test def testBitwiseOr_0_0(): Unit = {
    //implicit val printCode = cps.macroFlags.PrintCode
    //implicit val debugLevel = cps.macros.flags.DebugLevel(5)
    var x = false
    var y = false
    var xCalled = false
    var yCalled = false
    val c = async[ComputationBound] {
      {
        xCalled = true; x
      } | {
        yCalled = true; y
      }
    }
    val r = c.run()
    assert(r.isSuccess)
    r match
      case Success(v) =>
        assert(xCalled == true)
        assert(yCalled == true)
      case Failure(ex) => throw ex
  }

  @Test def testBitwiseOr_0_1(): Unit = {
    //implicit val printCode = cps.macroFlags.PrintCode
    //implicit val debugLevel = cps.macros.flags.DebugLevel(5)
    var x = true
    var y = true
    var xCalled = false
    var yCalled = false
    val c = async[ComputationBound] {
      {
        xCalled = true; x
      } | await(T1.cbt {
        yCalled = true; y
      })
    }
    val r = c.run()
    assert(r.isSuccess)
    r match
      case Success(v) =>
        assert(v == true)
        assert(xCalled == true)
        assert(yCalled == true)
      case Failure(ex) => throw ex
  }

  @Test def testBitwiseOr_1_0(): Unit = {
    //implicit val printCode = cps.macroFlags.PrintCode
    //implicit val debugLevel = cps.macros.flags.DebugLevel(5)
    var x = false
    var y = false
    var xCalled = false
    var yCalled = false
    val c = async[ComputationBound] {
      await(T1.cbt {
        xCalled = true; x
      }) | {
        yCalled = true; y
      }
    }
    val r = c.run()
    assert(r.isSuccess)
    r match
      case Success(v) =>
        assert(v == false)
        assert(xCalled == true)
        assert(yCalled == true)
      case Failure(ex) => throw ex
  }

  @Test def testBitwiseOr_1_1(): Unit = {
    //implicit val printCode = cps.macroFlags.PrintCode
    //implicit val debugLevel = cps.macros.flags.DebugLevel(5)
    var x = 1
    var y = 1
    var xCalled = false
    var yCalled = false
    val c = async[ComputationBound] {
      await(T1.cbt {
        xCalled = true; x
      }) | await(T1.cbt {
        yCalled = true; y
      })
    }
    val r = c.run()
    assert(r.isSuccess)
    r match
      case Success(v) =>
        assert(v == 1)
        assert(xCalled == true)
        assert(yCalled == true)
      case Failure(ex) => throw ex
  }


}
