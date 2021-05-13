package cps.streamlike

import cps._

sealed trait LazyStream[+T]:
   def map[S](f:T=>S): LazyStream[S]
   def flatMap[S](f: T=>LazyStream[S]): LazyStream[S]
   def append[S >: T](x: LazyStream[S]):LazyStream[S]

case class LazyStreamCons[+T](head: T, tailFun: () => LazyStream[T]) extends LazyStream[T]:

   def map[S](f: T=>S) = LazyStreamCons(f(head), () => tailFun().map(f) )

   def flatMap[S](f: T=> LazyStream[S]) = 
      f(head) match
        case LazyStreamNil => LazyStreamNil
        case LazyStreamCons(h1,t1) => LazyStreamCons(h1, () => t1().append(tailFun().flatMap(f)))

   def append[S >: T](x: LazyStream[S]) =
      LazyStreamCons(head, () => tailFun().append(x))

case object LazyStreamNil extends LazyStream[Nothing]:
   def map[S](f: Nothing=>S) = LazyStreamNil
   def flatMap[S](f: Nothing => LazyStream[S]) = LazyStreamNil
   def append[S](x: LazyStream[S]) = x


object LazyStreamCpsMonad extends CpsMonad[LazyStream]:

  def pure[A](a:A) = LazyStreamCons(a, () => LazyStreamNil)

  def map[A,B](fa:LazyStream[A])(f: A=>B): LazyStream[B] = fa.map(f)

  def flatMap[A,B](fa:LazyStream[A])(f: A=>LazyStream[B]): LazyStream[B] = fa.flatMap(f)


given CpsMonad[LazyStream] = LazyStreamCpsMonad

