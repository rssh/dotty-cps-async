package cpstest

import cps.*
import cps.monads.{*, given}
import cps.util.FutureCompleter
import org.junit.Test


import java.util.concurrent.atomic.AtomicReference
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.*
import scala.util.control.NonFatal

/**
 * Provide delimited continuation
 * @tparam F - effect monad
 *           R - result type
 *           A - type of value
 */
sealed trait CpsContT[F[_],R,+A] {

   def run(k: Try[A] => F[R]): F[R]

   def map[B](f: A=>B): CpsContT[F,R,B] =
     CpsContT.CallbackAcceptor((cb: Try[B] => F[R]) => this.run {
       case Success(a) => cb(Success(f(a)))
       case Failure(ex) => cb(Failure(ex))
     })

   def flatMap[B](f: A => CpsContT[F, R, B]): CpsContT[F, R, B] =
      CpsContT.CallbackAcceptor((cb: Try[B] => F[R]) => this.run {
        case Success(a) => f(a).run(cb)
        case Failure(ex) => cb(Failure(ex))
      })

   def flatMapTry[B](f: Try[A] => CpsContT[F, R, B]): CpsContT[F, R, B] =
      CpsContT.CallbackAcceptor((cb: Try[B] => F[R]) => this.run(ta => f(ta).run(cb)))

}

extension [F[_],R](self: CpsContT[F,R,R])
  def observe(using CpsTryMonad[F]): F[R] = CpsContT.observe(self)

object CpsContT {

  type CpsContR[F[_]] = [R] =>> CpsContT[F,R,R]

  case class CallbackAcceptor[F[_],R,A](fun: (Try[A]=>F[R])=>F[R]) extends CpsContT[F,R,A] {

    def run(k: Try[A] => F[R]): F[R] =
      fun(k)

  }

  def promote[F[_]:CpsTryMonad,R](fr: F[R]): CpsContT[F,R,R] = {
    CallbackAcceptor(cb => summon[CpsTryMonad[F]].flatMapTry(fr)(cb) )
  }

  def observe[F[_]:CpsTryMonad,R](w: CpsContT[F,R,R]):F[R] = {
    w.run(x => summon[CpsTryMonad[F]].fromTry(x))
  }

  def liftF[F[_]:CpsTryMonad,R,A](fa: F[A]): CpsContT[F,R,A] =
    CallbackAcceptor((cb: Try[A]=>F[R]) => summon[CpsTryMonad[F]].flatMapTry(fa)(cb) )

  def resetT[F[_]:CpsTryMonad, R, A](op: CpsContT[F, A, A]): CpsContT[F, R, A] = {
      liftF[F,R,A](observe(op))
  }

  def shiftTryT[F[_]:CpsTryMonad, R, A](k: (Try[A] => F[R])=>CpsContT[F,R,R] ): CpsContT[F, R, A] = {
      CallbackAcceptor[F,R,A] { cb =>
        observe(k(cb))
      }
  }

  def shiftT[F[_] : CpsTryMonad, R, A](k: (A => F[R]) => CpsContT[F, R, R]): CpsContT[F, R, A] = {
    CallbackAcceptor[F, R, A] { cb =>
      observe(k(a => cb(Success(a))))
    }
  }

  def shiftF[F[_] : CpsTryMonad, R, A](k: (A => F[R]) => F[R]): CpsContT[F, R, A] = {
    CallbackAcceptor[F, R, A] { cb =>
      k(a => cb(Success(a)))
    }
  }

  // To allow awaits inside async block arrount shift.
  def shiftF_async[F[_]: CpsTryMonad,R, A](k: (A => F[R]) => CpsContT[F,R,F[R]]): CpsContT[F, R, A] = {
    CallbackAcceptor[F, R, A] { cb =>
      k(a=>cb(Success(a))).run{
        case Success(fa) => fa
        case Failure(ex) => summon[CpsTryMonad[F]].error(ex)
      }
    }
  }



  def pure[F[_],R,A](a:A)(using m:CpsTryMonad[F]): CpsContT[F,R,A] =
    CallbackAcceptor((cb: Try[A]=>F[R]) => cb(Success(a)) )

  class CpsContTMonad[F[_]:CpsTryMonad,R] extends CpsTryMonad[[T] =>> CpsContT[F,R,T]] with CpsTryMonadInstanceContext[[T] =>> CpsContT[F,R,T]] {

    def pure[A](a:A): CpsContT[F,R,A] =
      CallbackAcceptor((cb: Try[A]=>F[R]) => cb(Success(a)))

    def error[A](ex:Throwable): CpsContT[F,R,A] =
      CallbackAcceptor((cb: Try[A]=>F[R]) => cb(Failure(ex)))

    override def fromTry[A](a: Try[A]): CpsContT[F,R,A] =
      CallbackAcceptor((cb: Try[A]=>F[R]) => cb(a) )

    def map[A,B](fa: CpsContT[F,R,A])(f: A=>B): CpsContT[F,R,B] =
      fa.map(f)

    def flatMap[A,B](fa: CpsContT[F,R,A])(f: A=>CpsContT[F,R,B]): CpsContT[F,R,B] =
      fa.flatMap(f)

    def flatMapTry[A,B](fa: CpsContT[F,R,A])(f: Try[A]=>CpsContT[F,R,B]): CpsContT[F,R,B] =
      fa.flatMapTry(f)


  }

  class CpsAsyncContTMonad[F[_] : CpsAsyncMonad, R] extends CpsContTMonad[F,R] with CpsAsyncMonad[[T] =>> CpsContT[F,R,T]] {

    def adoptCallbackStyle[A](source: (Try[A] => Unit) => Unit): CpsContT[F, R, A] =
      CallbackAcceptor((cb: Try[A] => F[R]) => {
        val fta = summon[CpsAsyncMonad[F]].adoptCallbackStyle(source)
        summon[CpsAsyncMonad[F]].flatMapTry(fta)(cb)
      })

  }

  given tryContT[F[_]:CpsTryMonad,R]: CpsContTMonad[F,R] = new CpsContTMonad[F,R]

  //given fromCont[F[_]:CpsTryMonad,R]: CpsMonadConversion[[T]=>>CpsContT[F,R,T],F] with
  //  def apply[A](fa: CpsContT[F,R,A]): F[A] = CpsContT.observe(fa)

  given toCont[F[_]:CpsTryMonad,R]: CpsMonadConversion[F,[T]=>>CpsContT[F,R,T]] with
    def apply[A](fa: F[A]): CpsContT[F,R,A] = CallbackAcceptor(
      (cb: Try[A]=>F[R]) => summon[CpsTryMonad[F]].flatMapTry(fa)(cb))

  //given asyncContT[F[_] : CpsAsyncMonad, R]: CpsAsyncContTMonad[F,R] = new CpsAsyncContTMonad[F,R]



}

// delimited continuation
//object DL {

  // unfortunately, it don't work inside other async block
  //  looks like inside resetF the context in awaits is resolved as in outer async block
  //  so, we need to use nested async block directly
  //transparent inline def resetF[F[_]:CpsTryMonad,R](inline op: CpsTryMonadInstanceContext[[X]=>>CpsContT[F,R,X]] ?=> R): F[R] =
  //    (async[[X]=>>CpsContT[F,R,X]] {
  //      op
  //    }).observe


//}



class TestCpsContT {

  @Test
  def checkShiftReset(): Unit = {
    val r = CpsContT.resetT[Future,Int,Int]{
        for{
          x <- CpsContT.pure(1)
          y <- CpsContT.shiftT[Future,Int,Int]{ k =>
                 //println(s"CpsCondT, shiftT,x=$x, k=$k, k(x+1)=${k(x+1)}")
                 CpsContT.liftF[Future,Int,Int](k(x+1))
               }
          z <- CpsContT.pure(y+3)
        } yield z
    }
    val r2 = CpsContT.observe(r).map{ x =>
      //println(s"CpsContT, r2 = $x")
      assert(x==5)
      x
    }
    FutureCompleter(r2)
  }

  @Test
  def checkShiftReset2(): Unit = {
    val r = CpsContT.resetT[Future, Int, Int] {
      for {
        x <- CpsContT.pure(1)
        y <- CpsContT.shiftF[Future, Int, Int] { k =>
          //println(s"CpsCondT:checkShiftReset2, shiftT,x=$x, k=$k, k(x+1)=${k(x+1)}")
          k(x + 1).flatMap(x => k(x + 1))
        }
        z <- CpsContT.pure(y + 3)
      } yield z
    }
    val r2 = CpsContT.observe(r).map { x =>
      //println(s"checkShiftReset2::CpsContT, r2 = $x")
      assert(x == 9)
      x
    }
    FutureCompleter(r2)
  }

  @Test
  def checkShiftReset3(): Unit = {
    val r = async[[T]=>>CpsContT[Future,Int,T]] {
      val x = 1
      val y = await(CpsContT.shiftF[Future,Int,Int]{ k =>
          //println(s"CpsCondT:checkShiftReset3, shiftT,x=$x, k=$k, k(x+1)=${k(x+1)}")
          val k1 = await(k(x + 1))
          val k2 = k(k1+1)
          k2
          //await(k(await(k(x + 1))))
      })
      y + 3
    }
    val r2 = CpsContT.observe(r).map { x =>
      //println(s"checkShiftReset3::CpsContT, r2 = $x")
      assert(x == 9)
      x
    }
    FutureCompleter(r2)
  }

  @Test
  def checkShiftReset4(): Unit = {
    val r = async[Future] {
      val x = 1
      val fr = async[[T]=>>CpsContT[Future,Int,T]] {
      //val fr = DL.resetF[Future,Int] {
        val y = await(CpsContT.shiftF[Future,Int,Int] { k =>
          //println(s"CpsCondT:checkShiftReset4, shiftT,x=$x, k=$k, k(x+1)=${k(x+1)}")
          val k1 = await(k(x + 1))
          val k2 = k(k1+1)
          k2
          //await(k(await(k(x + 1))))
        })
        y + 3
      }
      await(fr.observe)
    }
    val r2 = r.map { x =>
      println(s"checkShiftReset4::CpsContT, r2 = $x")
      assert(x == 9)
      x
    }
    FutureCompleter(r2)
  }


}