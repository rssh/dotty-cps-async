package cps.injection

import cps.*


trait AsyncInjectionCarrier[F[_],SET] {

    def get[T](using Contains[T,SET]): F[T]

}


trait InjectionCarrier[SET] {

  def get[T](using Contains[T,SET]): T

}


trait Contains[T,SET] {

  inline def get(s:SET): T

}

object Contains {

  given contains[T, S<:Tuple]: Contains[T, T *: S] with {
     inline def get(s: T *: S): T = s.head
  }

  given containsRest[T, HEAD, TAIL <: Tuple](using Contains[T,TAIL]) : Contains[T, HEAD *: TAIL] with {
     inline def get(s: HEAD *: TAIL): T = summon[Contains[T,TAIL]].get(s.tail)
  }

  given containsThemself[T]: Contains[T,T] with {
     inline def get(s: T): T = s
  }

}


type ALL = ALL.ALL

object ALL {
  opaque type ALL = ALL.type

  given allContains[T] : Contains[T,ALL] with  {
    inline def get(s: ALL): T = ??? // <work obly from macros>
  }

}

type SET = Tuple | ALL



object AsyncInjectionCarrier {

  /*
    def run[F[_],S,R](s:S)(f: InjectionCarrier[F,S] => F[R] )  = {
       ???
    }

    given injectCpsTryMonad[F[_],A](using CpsTryMonad[F]): CpsTryMonad[[X] =>> InjectionCarrier[F,A]=>F[X]] =
           ???

    given injectAddConversion[F[_],A,B <: Tuple]
      : CpsMonadConversion[ [X] =>> InjectionCarrier[F, A     ] => F[X],
                            [Y] =>> InjectionCarrier[F, A *: B] => F[Y]
                           ]  = new CpsMonadConversion {
                             override def apply[T](ft: InjectionCarrier[F,A] => F[T]):
                                                       (InjectionCarrier[F,A *: B] => F[T]) =
                                ???
                           }
  */

}

type AsyncInject[SET] = [F[_]] =>> AsyncInjectionCarrier[F,SET]


  /*
  class CpsMonadInjection[F[_],SET](val monad: CpsTryMonad[F]) extends CpsTryMonad[[X] =>> Provider[SET] => F[X]] {

      def pure [A](a:A): Provider[SET] => F[A] = (provider: Provider[SET]) => monad.pure(a)

      def map[A,B](fa: Provider[SET] => F[A])(f: A => B): Provider[SET] => F[B] =
         (provider: Provider[SET]) => monad.map(fa(provider))(f)

      def flatMap[A,B](fa: Provider[SET] => F[A])(f: A => Provider[SET] => F[B]): Provider[SET] => F[B] =
          (provider: Provider[SET]) => monad.flatMap(fa(provider))(a => f(a)(provider))

  }
  */

