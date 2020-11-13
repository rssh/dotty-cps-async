package cps

import org.junit.{Test,Ignore}
import org.junit.Assert._

import scala.quoted._
import scala.util.Success


class TestBS1Try:


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

  @Test def try_rethrow_tail(): Unit = 
     implicit val printCode = cps.macroFlags.PrintCode
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




