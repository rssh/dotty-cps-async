package cps

import org.junit.{Test,Ignore}
import org.junit.Assert._

import scala.quoted._
import scala.util.Success


import cps.syntax.*
import cps.macros.flags.*
import cps.plugin.annotation.CpsDebugLevel
//import cps.testconfig.given
given UseCompilerPlugin.type = UseCompilerPlugin


def TestCBS1Apply_toplevelfun(x: =>Int):Int = x + x


//@CpsDebugLevel(20)
class TestCBS1Apply3m3:


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



