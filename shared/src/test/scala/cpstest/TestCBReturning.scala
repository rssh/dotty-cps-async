package cpstest

import org.junit.{Test,Ignore}
import org.junit.Assert._

import scala.util.*
import scala.util.control.NonFatal
import scala.util.control.NonLocalReturns.*


import cps.*
import cps.testconfig.given



class TestCBSReturning:

  
  @Test def testReturning1t(): Unit = {
     //implicit val printCode = cps.macroFlags.PrintCode
     //implicit val debugLevel = cps.macros.flags.DebugLevel(5)
     val condition = true 
     val c = async[ComputationBound] {
         returning {
            if (condition) then
               throwReturn(1)
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

  @Test def testReturning1f(): Unit = {
     //implicit val printCode = cps.macros.flags.PrintCode
     //implicit val debugLevel = cps.macros.flags.DebugLevel(5)
     val condition = false 
     val c = async[ComputationBound] {
         returning {
            if (condition) then
               throwReturn(1)
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
      returning {
        if (true) then
          throwReturn(1)
        3
      }
    }
    val r = c.run()
     assert(r.isSuccess)
     r match
       case Success(v) => assert(v == 1)
       case Failure(ex) => throw ex
  }

  @Test def testSyncNoReturn(): Unit = {
    //implicit val printCode = cps.macros.flags.PrintCode
    //implicit val debugLevel = cps.macros.flags.DebugLevel(10)
    val c = async[ComputationBound] {
      returning {
        3
      }
    }
    val r = c.run()
     assert(r.isSuccess)
     r match
       case Success(v) => assert(v == 3)
       case Failure(ex) => throw ex
  }


  @Test def testReturning2ft(): Unit = {
     //implicit val printCode = cps.macros.flags.PrintCode
     //implicit val printTree = cps.macros.flags.PrintTree
     //implicit val debugLevel = cps.macros.flags.DebugLevel(5)
     val c1 = false
     val c2 = true 
     val c = async[ComputationBound] {
         returning {
            if (c1) then
               throwReturn(1)
            await(T1.cbi(2))
            if (c2) then
                throwReturn(2)
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

  @Test def testReturning3ft(): Unit = {
     //implicit val printCode = cps.macros.flags.PrintCode
     //implicit val printTree = cps.macros.flags.PrintTree
     //implicit val debugLevel = cps.macros.flags.DebugLevel(5)
     val c1 = false
     val c2 = false
     val c = async[ComputationBound] {
         returning {
            if (c1) then
               throwReturn(1)
            await(T1.cbi(2))
            if (c2) then
                throwReturn(2)
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

  @Test def testReturningFromTry(): Unit = {
     //implicit val printCode = cps.macros.flags.PrintCode
     //implicit val printTree = cps.macros.flags.PrintTree
     //implicit val debugLevel = cps.macros.flags.DebugLevel(15)
     val c = async[ComputationBound] {
         returning {
            try   
              await(T1.cbi(2))
              throwReturn(2)
            catch 
              case NonFatal(ex) =>
                8
         }
     }
     val r = c.run()
     //println(s"r == $r")
     assert(r.isSuccess)
     r match
       case Success(v) => assert(v == 2)
       case Failure(ex) => throw ex
  }

  transparent inline def earlyReturn[T](t:T)(using ReturnThrowable[T]):Nothing =
    throwReturn(t)

  @Test def inlineSubstForThrowReturn(): Unit = {
      val condition1 = true
      val c = async[ComputationBound] {
        returning {
           await(T1.cbi(2))
           if (condition1) {
             earlyReturn(1)
           } 
           10
        }
      }
      val r = c.run()
      r match
        case Success(v) => assert(v==1)
        case Failure(ex) => throw ex;
  }

  
