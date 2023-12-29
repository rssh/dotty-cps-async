package logic

import cps.*

import scala.util.Try


class LazyValue[A](value: =>A) {
  def get: A = value
  def getFun: () => A = () => value
}

object LazyValue {

   def apply[A](value: =>A): LazyValue[A] = new LazyValue(value)

   given CpsTryEffectMonad[LazyValue] with CpsTryMonadInstanceContext[LazyValue] with

      override def pure[A](a:A): LazyValue[A] = LazyValue(a)

      override def map[A,B](fa:LazyValue[A])(f: A=>B): LazyValue[B] =
         LazyValue(f(fa.get))

      override def flatMap[A,B](fa:LazyValue[A])(f: A=>LazyValue[B]): LazyValue[B] =
         LazyValue(f(fa.get).get)

      override def flatMapTry[A, B](fa: LazyValue[A])(f: Try[A] => LazyValue[B]): LazyValue[B] = {
          LazyValue(f(Try(fa.get)).get)
      }

      override def error[A](e: Throwable): LazyValue[A] =
         LazyValue(throw e)

}

class LazyT[F[_]:CpsTryMonad,A](value: =>F[A]) {

  def get: F[A] = value

  def getFun: () => F[A] = () => value

}

object LazyT {

  class CpsLazyEffect[F[_]:CpsTryMonad] extends CpsTryEffectMonad[[X]=>>LazyT[F,X]] with CpsTryMonadInstanceContext[[X]=>>LazyT[F,X]] {

    override def pure[A](a: A): LazyT[F, A] = LazyT(summon[CpsTryMonad[F]].pure(a))

    override def map[A, B](fa: LazyT[F, A])(f: A => B): LazyT[F, B] =
      LazyT(summon[CpsTryMonad[F]].map(fa.get)(f))

    override def flatMap[A, B](fa: LazyT[F, A])(f: A => LazyT[F, B]): LazyT[F, B] =
      LazyT(summon[CpsTryMonad[F]].flatMap(fa.get)(f(_).get))

    override def flatMapTry[A, B](fa: LazyT[F, A])(f: Try[A] => LazyT[F, B]): LazyT[F, B] = {
      LazyT(summon[CpsTryMonad[F]].flatMapTry(fa.get)(f(_).get))
    }

    override def error[A](e: Throwable): LazyT[F, A] =
      LazyT(summon[CpsTryMonad[F]].error(e))

  }

  given cpsLazyEffect[F[_]:CpsTryMonad]: CpsTryEffectMonad[[X]=>>LazyT[F,X]] = CpsLazyEffect[F]()

}
