package cpstest

import scala.annotation.*
import cps.*

sealed trait MyList[+A] {

  def map[B](f: A=>B ): MyList[B]

//  def map_async[B,F[_]](m: CpsMonad[F])(f: A=>F[B] ): F[List[B]] = ???

}


case object MyNil extends MyList[Nothing] {

  def map[B](f: Nothing=>B ): MyList[B] = MyNil

//  def map_async[B,F[_]](m: CpsMonad[F])(f: Nothing => B): m.pure(MyNil)

}

case class MyCons[A](head:A, tail:MyList[A]) extends MyList[A]{

  def map[B](f: A=>B ): MyList[B] = {
    MyCons(f(head), tail.map(f))
  }

//  def map_async[B](m: CpsMonad[F])(f: A => F[B]): F[List[B]] = async[F]{
//    MyCons(await(f(head)), await(tail.map_asyn(f)) )
//  }

}

import scala.concurrent.*
import cps._
import cps.monads.{*,given}


@experimental
object MyListExample {

  object network {
    def fetch(x:String): CpsDirect[Future] ?=> String =
      ???
  }

  //def myFunction1(l:MyList[String]): Future[List[String]] = async[Future] {
  //   val otherList: List[Int] = l.map(url => await(network.fetch(url)))
  //   println(otherList)
  //}

  //A=>B
  // A=>F[B]

  //def myFunction1_t(l: MyList[String]): Future[List[String]] = {
  //  m.pure(l).flatMap{ l =>
  //     l.map_async(m){
  //       url => network.fetch(url).map{ otherList =>
  //         println(otherList)
  //       }
  //     })
  //  }
    //val otherList: List[Int] = l.map(url => await(network.fetch(url)))
    //println(otherList)
  //}

  //def fetchList(urls: List[String]): Future[List[String]] = async[Future] {
  //  urls.map(url => network.fetch(url))
  //}

  //
  def fetchList(urls: List[String]): CpsDirect[Future] ?=> List[String] =
      urls.map(url => network.fetch(url) )

  //def myFunction2_compiler(l: MyList[String]): CpsMonadContext[Future] ?=> Future[MyList[String]] = ...



}

//  EffectLike[Effects] =  [T] =>> EffectLikeM[Effects,T]
//

