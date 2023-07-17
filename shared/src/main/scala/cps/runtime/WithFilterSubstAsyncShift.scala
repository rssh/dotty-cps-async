package cps.runtime

import cps._
import scala.collection._
import scala.reflect.ClassTag
import scala.collection.mutable.ArrayBuilder
import scala.collection.mutable.Builder


/**
 * WithFilter should be handled in a special way, because it is impossible to access underlaying collection
 * from withFilter,  so it's impossible to write async version of withFilter functions, because they require
 * eration over original collection.
 * @param ca original colleaction
 * @param p predicate
 * @param csf Asy
 * @tparam A
 * @tparam C
 * @tparam CA
 */
class WithFilterSubstAsyncShift[A, C[X] <: Iterable[X] & IterableOps[X,C,C[X]], CA <: C[A]]( 
                                                                 ca: CA,
                                                                 p: A=>Boolean,
                                                                 csf: IterableOpsAsyncShift[A,C,CA])
                                      {

  def foreach[F[_],U](m: CpsMonad[F])(f: A => F[U]): F[Unit] = 
    csf.foreach(ca,m){ v =>
      if p(v) then
         m.map(f(v))(x => ())
      else
         m.pure(())
    }
    

  def map[F[_], B](m: CpsMonad[F])(f: A=> F[B]): F[C[B]] = 
    csf.flatMap(ca,m) { v =>
       if p(v) then
         m.map(f(v))(x => Some(x))
       else
         m.pure(None)
    }
    
  def flatMap[F[_], B](m: CpsMonad[F])(f: A=> F[IterableOnce[B]]):F[C[B]] = 
    csf.flatMap(ca,m) { v =>
       if p(v) then
          f(v)
       else
          m.pure(None)
    }


  def withFilter[F[_]](m: CpsMonad[F])(p1: A => F[Boolean]): DelayedWithFilter[F,A,C,CA] =
      DelayedWithFilter(ca,m, { a => 
        if p(a) then
           p1(a)
        else
           m.pure(false)
      })


}

  

