package cps.runtime

import cps._
import scala.collection._
import scala.reflect.ClassTag
import scala.collection.mutable.ArrayBuilder

abstract class IterableAsyncShift[A,C <: [X]=>>Iterable[X] ] extends AsyncShift[C[A]] {

  def foreach[F[_],U](c: C[A], monad: CpsMonad[F])(f: A => F[U]): F[Unit] = {
     var r:F[Unit] = monad.pure(())
     c.foreach{ a =>
       val b = f(a)
       r = monad.flatMap(r)(_ => monad.map(b)(_ =>()) )   
     }
     r
  }

  def map[F[_], B](c: C[A], monad: CpsMonad[F])(f: A=> F[B]):F[C[B]] 

  def flatMap[F[_], B](c: C[A], monad: CpsMonad[F])(f: A=> F[IterableOnce[B]]):F[C[B]] 

}

class ListAsyncShift[A] extends IterableAsyncShift[A,List] with AsyncShift[List[A]] {

  override def map[F[_], B](c: List[A], monad: CpsMonad[F])(f: A=> F[B]):F[List[B]] =
    c match
      case Nil => monad.pure(Nil)
      case head::tail => val hbf = f(head)
                         val tbf = map(tail,monad)(f)
                         monad.flatMap(hbf)(hb => 
                               monad.map(tbf)(tb => hb::tb))

  override def flatMap[F[_], B](c: List[A], monad: CpsMonad[F])(f: A=> F[IterableOnce[B]]):F[List[B]]  = ???

}

