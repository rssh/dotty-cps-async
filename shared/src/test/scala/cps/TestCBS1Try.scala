package cps

import org.junit.{Test,Ignore}
import org.junit.Assert._

import scala.quoted._
import scala.util.Success
import scala.util.Failure
import cps.testconfig.given


class TestСBS1Try:


  @Test def try_00n_p(): Unit = 
     val c = async[ComputationBound]{
        var x = 1
        try {
          //println(10)
          x = 2
        }catch{
          case ex:Exception => //ex.printStackTrace()
          x = 3
        }
        x
     }
     assert(c.run() == Success(2))


  @Test def try_00n_f(): Unit = 
     val c = async[ComputationBound]{
        var x = 1
        try {
          //println(10)
          throw new RuntimeException("AAA")
          x = 2
        }catch{
          case ex:Exception => // ex.printStackTrace()
          x = 3
        }
        x
     }
     assert(c.run() == Success(3))


  @Test def try_10n_p(): Unit = 
     val c = async{
        var x = 1
        try {
          x = await(T1.cbi(2))
        }catch{
          case ex:Exception => // ex.printStackTrace()
                    x = 3
        }
        x
     }
     assert(c.run() == Success(2))

  @Test def try_10n_f(): Unit = 
     val c = async{
        var x = 1
        try {
          x = await(T1.cbi(2))
          throw new RuntimeException("AAA")
        }catch{
          case ex:Exception => // ex.printStackTrace()
                    x = 3
        }
        x
     }
     assert(c.run() == Success(3))


  @Test def try_01n_p(): Unit = 
     val c = async{
        var x = 1
        try {
          x = 2
        } catch {
          case ex:Exception => 
                    x = await( T1.cbi(3) )
        }
        x
     }
     assert(c.run() == Success(2))

  @Test def try_01n_f(): Unit = 
     val c = async{
        var x = 1
        try {
          x = 2
          throw new RuntimeException("AAA")
        } catch {
          case ex:Exception => 
                    x = await( T1.cbi(3) )
        }
        x
     }
     assert(c.run() == Success(3))

  @Test def try_11n_p(): Unit = 
     val c = async{
        var x = 1
        try {
          x = await(T1.cbi(2))
        } catch {
          case ex:Exception => 
                    x = await( T1.cbi(3) )
        }
        x
     }
     assert(c.run() == Success(2))

  def failSyncOp(): Unit =
        throw new Exception("failSyncOp")

  @Test def try_rethrow_incase(): Unit = 
     //implicit val printCode = cps.macroFlags.PrintCode
     //implicit val debugLevel = cps.macroFlags.DebugLevel(20)
     var x = 0
     val c = async{
        x = await(T1.cbi(1))
        try {
          failSyncOp();
          x = 3;
        } catch {
          case ex:Exception => 
            x = 2  
            throw ex;
        }
        x
     }
     val r = c.run()
     assert(r.isFailure)
     assert(x == 2)

  @Test def try_rethrow_in_finalizer(): Unit = 
     //implicit val printCode = cps.macroFlags.PrintCode
     //implicit val debugLevel = cps.macroFlags.DebugLevel(20)
     var x = 0
     val c = async{
        x = await(T1.cbi(1))
        try {
          failSyncOp();
          x = 3;
        } finally {
            x = 2  
            throw new RuntimeException("BBB");
        }
        x
     }
     val r = c.run()
     assert(r.isFailure)
     r match
       case Failure(ex) =>
         assert(ex.getMessage == "BBB")
         assert(x == 2)
       case _ =>
         assert(false)


  @Test def tryAsyncInFinalizer(): Unit = 
     //implicit val printCode = cps.macroFlags.PrintCode
     //implicit val debugLevel = cps.macroFlags.DebugLevel(20)
     var x = 0
     val c = async{
        x = await(T1.cbi(1))
        try {
          failSyncOp();
          x = 3;
        } finally {
            x = await(T1.cbi(2))
        }
        x
     }
     val r = c.run()
     assert(r.isFailure)
     assert(x == 2)

  @Test def tryAsyncCaseDefFinalizer(): Unit = 
     //implicit val printCode = cps.macroFlags.PrintCode
     //implicit val debugLevel = cps.macroFlags.DebugLevel(20)
     var x = 0
     var y = 0
     val c = async{
        x = await(T1.cbi(1))
        try {
          failSyncOp()
          x = 3
        } catch {
           case ex: Throwable =>
              x = 2
              throw ex
        } finally {
            y = await(T1.cbi(2))
        }
        x
     }
     val r = c.run()
     assert(r.isFailure)
     assert(x == 2)
     assert(y == 2)


  @Test def tryInMatch1(): Unit = 
     //implicit val printCode = cps.macroFlags.PrintCode
     var wasInCatch: Boolean = false
     val x: Int = 1
     val c = async {
        await(T1.cbi(2)) match
          case 2 =>
            try
              x match
               case 1 =>
                 failSyncOp()
               case _ =>
                 throw new RuntimeException("AAA") 
            catch
              case ex: Throwable =>
                wasInCatch = true
                throw ex
     }
     val r = c.run()
     assert(r.isFailure)
     assert( wasInCatch )

  class Ex1 extends RuntimeException("Ex1")
  class Ex2 extends RuntimeException("Ex2")

  @Test def tryThrowingOther(): Unit = {
      val c = async {
         try {
            await(T1.cbi(2))
            if true then
               throw Ex2()
         }catch{
            case ex: Ex1 => 1 
         }
         2
      }
      val r = c.run()
      assert(r.isFailure)
      r match
        case Failure(ex) =>
          assert(ex.isInstanceOf[Ex2])
        case _ =>
          assert(false)
  }
     



