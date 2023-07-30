package cps

import org.junit.{Test,Ignore}
import org.junit.Assert._

import scala.quoted._
import scala.util.Success
import scala.util.Failure
import cps.testconfig.given


class TestСBS1Try:


  def failSyncOp(): Unit =
        throw new Exception("failSyncOp")

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

  /*
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
     */



