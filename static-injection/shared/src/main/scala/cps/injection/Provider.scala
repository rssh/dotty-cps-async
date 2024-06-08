package cps.injection

import cps.*

trait Provider[SET] {

    def get[T](using Getter[SET,T]): T

}

trait Getter[SET,T] {
    def get(set: SET): T
}

//trait InjectionEffect[F[_],SET]

trait InjectionCarrier[F[_],SET] {

    def provider: Provider[SET]

}

object InjectionCarrier {

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


}

type Inject[SET] = [F[_]] =>> InjectionCarrier[F,SET]

/*
class CpsMonadInjection[F[_],SET](val monad: CpsTryMonad[F]) extends CpsTryMonad[[X] =>> Provider[SET] => F[X]] {

    def pure [A](a:A): Provider[SET] => F[A] = (provider: Provider[SET]) => monad.pure(a)

    def map[A,B](fa: Provider[SET] => F[A])(f: A => B): Provider[SET] => F[B] =
       (provider: Provider[SET]) => monad.map(fa(provider))(f)

    def flatMap[A,B](fa: Provider[SET] => F[A])(f: A => Provider[SET] => F[B]): Provider[SET] => F[B] =
        (provider: Provider[SET]) => monad.flatMap(fa(provider))(a => f(a)(provider))

}
*/

//  get laster dotty, create
//  (implitci)
def testFun[F[_]]  =  (x:Int) =>  {
   ???
}

// xxx
def testFun1[F[_]]: InjectionCarrier[F,Int] => F[String]  = (x: InjectionCarrier[F,Int]) => {
   ???
}

/*
def testFun2[F[_]:CpsTryMonad]  =  reify[[X] =>> InjectionCarrier[F, Int *: Boolean] => F[X]]{
    val q = reflect(testFun1)
}
*/

