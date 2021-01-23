package cps.runtime.util

import scala.util._
import cps._

class TryAsyncShift[T] extends AsyncShift[Try[T]]:

   //
   //def collect[F[_],U](o:Try[T],m:CpsMonad[F]](pf: PartialFunction[T,U]):F[Try[U]] = 

   def filter[F[_]](o:Try[T],m:CpsMonad[F])(p: T=>F[Boolean]): F[Try[T]] =
         o match 
           case Success(t) =>
                 m.map(p(t)){ r =>
                    o.filter(_ => r)
                 }
           case Failure(ex) => m.pure(Failure(ex))

   def flatMap[F[_],U](o:Try[T], m:CpsMonad[F])(f: (T)=>F[Try[U]]):F[Try[U]] =
         o match
           case Success(t) => f(t)
           case Failure(ex) => m.pure(Failure(ex))

   def fold[F[_],U](o: Try[T], m:CpsMonad[F])(fa: (Throwable)=>F[U],
                                              fb: (T)=>F[U]): F[U] =
         o.fold(fa,fb)

   def foreach[F[_],U](o: Try[T], m:CpsMonad[F])(f: (T)=>F[U]):F[Unit] =
         o match
           case Success(t) => m.map(f(t))(_ => ())
           case Failure(ex) => m.pure(())

   def getOrElse[F[_], U>:T](o:Try[T], m:CpsMonad[F])(default: ()=>F[U]): F[U] =
         o match
           case Success(t) => m.pure(t)
           case Failure(ex) => default()
   
   def map[F[_],U](o: Try[T], m:CpsMonad[F])(f: (T)=>F[U]): F[Try[U]] =
         o match
           case Success(t) => m.map(f(t))(x=>Success(x))
           case Failure(ex) => m.pure(Failure(ex))

   def orElse[F[_], U>:T](o:Try[T], m:CpsMonad[F])(default: ()=>F[Try[U]]): F[Try[U]] =
         o match
           case Success(t) => m.pure(Success(t))
           case Failure(ex) => default()

   def recover[F[_], U>:T](o:Try[T],m: CpsMonad[F])(pf: PartialFunction[Throwable,F[U]]):F[Try[U]] =
         o match
           case Success(t) => m.pure(Success(t))
           case Failure(ex) => m.map(pf(ex))(u => Success(u))

   def recoverWith[F[_], U>:T](o:Try[T],m: CpsMonad[F])(
                      pf: PartialFunction[Throwable,F[Try[U]]]):F[Try[U]] =
         o match
           case Success(t) => m.pure(Success(t))
           case Failure(ex) => pf(ex)



object TryModuleAsyncShift extends AsyncShift[Try.type]:

   def apply[F[_],T](o:Try.type, m:CpsTryMonad[F])(r: ()=>F[T]):F[Try[T]] =
         m.mapTry(r())(identity)


