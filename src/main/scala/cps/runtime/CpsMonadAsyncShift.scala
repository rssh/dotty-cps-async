package cps.runtime

import cps._
import scala.collection._
import scala.reflect.ClassTag
import scala.collection.mutable.ArrayBuilder

//TODO: not needed
class CpsMonadSelfAsyncShift[F[_], M<:CpsMonad[F]] extends AsyncShift[M]: 

   def map[A,B](fMonad:CpsMonad[F])(fa:F[A])(f: A=>F[B]):F[F[B]] =
        fMonad.map(fa)(f) 
       
   def flatMap[A,B](fMonad:CpsMonad[F])(fa:F[A])(f: A=>F[F[B]]):F[F[B]] =
        fMonad.flatMap(fa)(f) 
