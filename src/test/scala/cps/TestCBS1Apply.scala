package cps

import org.junit.{Test,Ignore}
import org.junit.Assert._

import scala.quoted._
import scala.util.Success

def TestCBS1Apply_toplevelfun(x: =>Int):Int = x + x

class TestCBS1Apply:


  @Test def apply_fun1(): Unit = {
     //implicit val printCode = cps.macroFlags.PrintCode
     //implicit val debugLevel = cps.macroFlags.DebugLevel(20)
     val c = async{
       await(T1.cbi(2)) + 3
     }
     assert(c.run() == Success(5))
  }

  class Zzz(zx:Int) {
     def ta[T](t:T):String =
          t.toString + zx.toString
 
     def byNameInt(x: =>Int):Int =
          zx + x + x

     def byNameCurriedIntInt(x: =>Int)(y: =>Int):Int =
           zx+x+x+y+y;

     def znt[T <: Any](x: =>T):String =
           x.toString + x.toString

     def ztCurried[T <: Any](x: T)(y: T):String =
           x.toString + y.toString + x.toString + y.toString

     def zntCurried[T <: Any](x: =>T)(y: =>T):String =
           x.toString + y.toString + x.toString + y.toString

     def shifted[F[_]](m: CpsMonad[F]): AsyncShifted[Zzz,F] = new AsyncShifted[Zzz,F] {

          def byNameInt(x: ()=>F[Int]): F[Int] = {
               m.flatMap(x()){ x1 =>
                 m.map(x()){ x2 =>
                    zx + x1 + x2
               } }
          }

     }

  }

  object Zzz {

    given shiftedZzz[F[_]](using m:CpsMonad[F]) as Conversion[Zzz,AsyncShifted[Zzz,F]] =
          zzz => zzz.shifted[F](m)

    //extension AsyncShifted



  }


  @Test def apply_fun2(): Unit = {
     //implicit val printCode = cps.macroFlags.PrintCode
     //implicit val debugLevel = cps.macroFlags.DebugLevel(20)
     val c = async{
       await(T1.cbi(2).map(new Zzz(_))).ta("qqq")
     }
     assert(c.run() == Success("qqq2"))
  }

  @Test def apply_funNamed(): Unit = {
     implicit val printCode = cps.macroFlags.PrintCode
     implicit val debugLevel = cps.macroFlags.DebugLevel(20)
     val c = async{
       //given Conversion[Zzz,AsyncShifted[Zzz,ComputationBound]] = z => z.shifted[ComputationBound](summon[CpsMonad[ComputationBound]])
       val conversion = summon[Conversion[Zzz, AsyncShifted[Zzz,ComputationBound]]]
       val zzz = new Zzz(3)
       var x = 0;
       zzz.byNameInt(await({ x=x+1; T1.cbi(2)}))
       x
     }
     assert(c.run() == Success(2))
  }

/*
  @Test @Ignore def apply_funGenericNamed(): Unit = {
     //implicit val printCode = cps.macroFlags.PrintCode
     //implicit val debugLevel = cps.macroFlags.DebugLevel(20)
     val c = async{
       val zzz = new Zzz(3)
       var x = 0;
       zzz.znt(await({ x=x+1; T1.cbi(2)}))
       x
     }
     // TODO: assert x
     assert(c.run() == Success(2))
  }
*/

  @Test def apply_funGenericCurried(): Unit = {
     //implicit val printCode = cps.macroFlags.PrintCode
     //implicit val debugLevel = cps.macroFlags.DebugLevel(20)
     val c = async{
       val zzz = new Zzz(3)
       var x = 0;
       val q = zzz.ztCurried(await({ x=x+1; T1.cbi(2)}))(await({x=x+1; T1.cbs("A")}))
       x
     }
     assert(c.run() == Success(2))
  }

/*
  @Test @Ignore def apply_funGenericByNameCurried(): Unit = {
     //implicit val printCode = cps.macroFlags.PrintCode
     //implicit val debugLevel = cps.macroFlags.DebugLevel(20)
     val c = async{
       val zzz = new Zzz(3)
       var x = 0;
       val q = zzz.zntCurried(await({ x=x+1; T1.cbi(2)}))(await({x=x+1; T1.cbs("A")}))
       x
     }
     assert(c.run() == Success(4))
  }
*/


