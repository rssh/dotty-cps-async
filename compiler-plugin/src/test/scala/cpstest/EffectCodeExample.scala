package cpstest



sealed trait MyList[+A] {

  def map[B](f: A=>B ): List[B]

  def map_async[B,F[_]](m: CpsMonad[F])(f: A=>F[B] ): F[List[B]]

}


case object MyNil extends MyList[Nothing] {

  def map[B](f: Nothing=>B ): List[B] = MyNil

  def map_async[B,F[_]](m: CpsMonad[F])(f: Nothing => B): m.pure(MyNil)

}

case class MyCons[A](head:A, tail:MyList[A]) {

  def map[B](f: A=>B ): List[B] = {
    MyCons(f(head), tail.map(f))
  }

  def map_async[B](m: CpsMonad[F])(f: A => F[B]): F[List[B]] = async[F]{
    MyCons(await(f(head)), await(tail.map_asyn(f)) )
  }

}

import scala.concurrent.*
import cps._
import cps.monads.{*,given}

/*
Monad {

  def pure
  def map

}
*/

object Test {

  object network {
    def fetch(x:String): MonadContext[Future] ?=> Future[String]
  }

  //def myFunction(l:MyList[String]): Future[List[String]] = async[Future] {
  //   val otherList: List[Int] = l.map(url => await(network.fetch(url)))
  //   println(otherList)
  //}

  def myFunction(l: MyList[String]): MonadContext[Future] ?=> MyList[String] =
  {
    val otherList: List[Int] = l.map(url => network.fetch(url) )
    println(otherList)
  }


  def myFunction1(l: MyList[String]) = {
    val m = summon[CpsMonad[Future]]
    l.map_async(url => network.fetch(url))
      .map { otherList: List[] =>
        println(otherList)
      }
  }

  def testFunction2(): Unit = {
     //val x = await(something)
     //v
     something.map { x =>
       y = x + 1
     }
  }


}

//  EffectLike[Effects] =  [T] =>> EffectLikeM[Effects,T]
//


object EffectCodeExample {


}
