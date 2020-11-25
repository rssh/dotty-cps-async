// part of dotty-cps-async
// (C) Ruslan Shevchenko, 2020
package cps.runtime.util

import scala.util._
import cps._

class EitherAsyncShift[A,B] extends AsyncShift[Either[A,B]]:

  
   def exists[F[_]](o:Either[A,B],m:CpsMonad[F])(p: B=>F[Boolean]): F[Boolean] =
       o match
          case Left(a) => m.pure(false)
          case Right(b) => p(b)

   def filterOrElse[F[_],A1 >: A](o:Either[A,B],m:CpsMonad[F])(p: B=>F[Boolean], zero: ()=>F[A1]): F[Either[A1,B]] =
         o match 
           case Left(a) => m.pure(Left(a))
           case Right(b) =>
                  m.flatMap(p(b)){ x =>
                       if (x) m.pure(Right(b)) else m.map(zero())(Left(_))
                  }

   def flatMap[F[_],A1 >:A, B1](o:Either[A,B], m:CpsMonad[F])(f: B => F[Either[A1,B1]]):F[Either[A1,B1]] =
         o match
           case Left(a) => m.pure(Left(a))
           case Right(b) => f(b)

   def fold[F[_],C](o: Either[A,B], m:CpsMonad[F])(fa: (A)=>F[C],
                                                   fb: (B)=>F[C]): F[C] =
         o.fold(fa,fb)

   def forall[F[_]](o: Either[A,B], m:CpsMonad[F])(p: (B)=>F[Boolean]):F[Boolean] =
         o match
           case Left(a) => m.pure(false)
           case Right(b) => p(b)

   def foreach[F[_],U](o: Either[A,B], m:CpsMonad[F])(f: (B)=>F[U]):F[Unit] =
         o match
           case Left(a) => m.pure(())
           case Right(b) => m.map(f(b))(_ => ())

   def getOrElse[F[_], B1 >: B](o:Either[A,B], m:CpsMonad[F])(default: ()=>F[B1]): F[B1] =
         o match
           case Left(a) => default()
           case Right(b) => m.pure(b)
   
   def map[F[_],B1](o: Either[A,B], m:CpsMonad[F])(f: (B)=>F[B1]): F[Either[A,B1]] =
         o match
           case Left(a) => m.pure(Left(a))
           case Right(b) => m.map(f(b))(Right(_))

   def orElse[F[_], A1>:A, B1>:B](o:Either[A,B], m:CpsMonad[F])(default: ()=>F[Either[A1,B1]]): F[Either[A1,B1]] =
         o match
           case Left(a) => default()
           case rb@Right(b) => m.pure(rb:Right[A1,B1])



class EitherLeftProjectionAsyncShift[A,B] extends AsyncShift[Either.LeftProjection[A,B]]:


   def exists[F[_]](o:Either.LeftProjection[A,B],m:CpsMonad[F])(p: A=>F[Boolean]): F[Boolean] =
        o.e match
          case Left(a) => p(a)
          case _ => m.pure(false)

   def filterToOption[F[_], B1](o:Either.LeftProjection[A,B],m:CpsMonad[F])(p: A=>F[Boolean]): F[Option[Either[A,B1]]] =
        o.e match
          case Left(a) => m.map(p(a))(x => if (x) Some(Left(a)) else None)
          case _ => m.pure(None)

   def flatMap[F[_], A1, B1 >: B](o:Either.LeftProjection[A,B],m:CpsMonad[F])
                                                                   (f: A=>F[Either[A1,B1]]): F[Either[A1,B1]] =
        o.e match
          case Left(a) => f(a)
          case Right(b) => m.pure(Right(b))

   def forall[F[_]](o:Either.LeftProjection[A,B],m:CpsMonad[F])(p: A=>F[Boolean]): F[Boolean] =
        o.e match
          case Left(a) => p(a)
          case _ => m.pure(true)

   def foreach[F[_],U](o:Either.LeftProjection[A,B],m:CpsMonad[F])(f: A=>F[U]): F[Unit] =
        o.e match
          case Left(a) => m.map(f(a))(_ => ())
          case _ => m.pure(())

   def getOrElse[F[_], A1 >: A](o:Either.LeftProjection[A,B], m:CpsMonad[F])(default: ()=>F[A1]): F[A1] =
        o.e match
          case Left(a) => m.pure(a)
          case _ => default()

   def map[F[_],A1](o: Either.LeftProjection[A,B], m:CpsMonad[F])(f: (A)=>F[A1]): F[Either[A1,B]] =
        o.e match
          case Left(a) => m.map(f(a))(Left(_))
          case Right(b) => m.pure(Right(b))




