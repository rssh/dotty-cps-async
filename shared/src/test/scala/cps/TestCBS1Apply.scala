package cps

import org.junit.{Test,Ignore}
import org.junit.Assert._

import scala.quoted._
import scala.util.Success

import cps.syntax.*
import cps.testconfig.given

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

     def byNameInt_async[F[_]](m: CpsMonad[F], x: ()=>F[Int]): F[Int] = {
               m.flatMap(x()){ x1 =>
                 m.map(x()){ x2 =>
                    zx + x1 + x2
               } }
     }

     def znt_async[F[_],T <: Any](m:CpsMonad[F],x:()=>F[T]): F[String] =
            given CpsMonad[F] = m
            for{ fx1 <- x()
                fx2 <- x()
            } yield fx1.toString + fx2.toString

     def zntCurried_async[F[_],T <: Any](m: CpsMonad[F], x: ()=>F[T])(y: ()=>F[T]): F[String] =
            given CpsMonad[F] = m
            for{ fx1 <- x()
                 fy1 <- y()
                 fx2 <- x()
                 fy2 <- y()
            } yield fx1.toString + fy1.toString + fx2.toString + fy2.toString

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
     //implicit val printCode = cps.macroFlags.PrintCode
     //implicit val debugLevel = cps.macroFlags.DebugLevel(20)
     val c = async{
       val zzz = new Zzz(3)
       var x = 0;
       zzz.byNameInt(await({ x=x+1; T1.cbi(2)})):Unit
       x
     }
     assert(c.run() == Success(2))
  }

  @Test def apply_funGenericNamed(): Unit = {
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

  @Test def apply_funGenericByNameCurried(): Unit = {
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

  class DifferentAsync[F[_]:CpsMonad](x: F[Int]) {

     def fvalue: F[Int] = x 

     def map(f: Int=>Int): DifferentAsync[F] =
          DifferentAsync(summon[CpsMonad[F]].map(x)(f))

     def mapAsync(f: Int=>F[Int]): DifferentAsync[F] =
          DifferentAsync(summon[CpsMonad[F]].flatMap(x)(f))


  }


  @Test def apply_mapAsyncSame(): Unit = {
     //implicit val printCode = cps.macroFlags.PrintCode
     //implicit val debugLevel = cps.macroFlags.DebugLevel(20)
     val c = async {
       val d1 = DifferentAsync[ComputationBound](T1.cbi(2))
       val d2 = d1.map(x => x+1)
       val d3 = d1.map(x => x+await(T1.cbi(2)))
       await(d3.fvalue)
     }
     assert(c.run() == Success(4))

  }


