package cps.injection.examples.randomgen.provider

import cps.injection.examples.randomgen.PureEffect

trait RandomProvider[F[_], T]{
  def provideMany(n: Int): F[Iterable[T]]
  def provideOne: F[T]
}

