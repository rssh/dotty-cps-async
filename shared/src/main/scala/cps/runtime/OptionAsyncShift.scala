package cps.runtime

import scala.util._
import cps._

class OptionAsyncShift[T] extends AsyncShift[Option[T]]:


   def filter[F[_]](o:Option[T],m:CpsMonad[F])(p: T=>F[Boolean]): F[Option[T]] =
         o match 
           case Some(t) =>
                 m.map(p(t)){ r =>
                    o.filter(_ => r)
                 }
           case None => m.pure(None)

   def flatMap[F[_],U](o:Option[T], m:CpsMonad[F])(f: (T)=>F[Option[U]]):F[Option[U]] =
         o match
           case Some(t) => f(t)
           case None => m.pure(None)

   def foreach[F[_],U](o: Option[T], m:CpsMonad[F])(f: (T)=>F[U]):F[Unit] =
         o match
           case Some(t) => m.map(f(t))(_ => ())
           case None => m.pure(())

   def getOrElse[F[_], U>:T](o:Option[T], m:CpsMonad[F])(default: ()=>F[U]): F[U] =
         o match
           case Some(t) => m.pure(t)
           case None => default()
   
   def map[F[_],U](o: Option[T], m:CpsMonad[F])(f: (T)=>F[U]): F[Option[U]] =
         o match
           case Some(t) => m.map(f(t))(x=>Some(x))
           case None => m.pure(None)

   def orElse[F[_], U>:T](o:Option[T], m:CpsMonad[F])(default: ()=>F[Option[U]]): F[Option[U]] =
         o match
           case Some(t) => m.pure(Some(t))
           case None => default()



