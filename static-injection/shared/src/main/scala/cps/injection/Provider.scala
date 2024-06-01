package cps.injection

import cps.*

trait Provider[SET] {

    def get[T](using Getter[SET,T]): T

}

trait Getter[SET,T] {
    def get(set: SET): T
}

//trait InjectionEffect[F[_],SET]

trait InjectionCarries[F[_],SET] {

    def provider: Provider[SET]

}

type Inject[SET] = [F[_]] =>> InjectionCarries[F,SET]


def testFun[F[_]: Inject[Boolean] ]: Unit = {

}