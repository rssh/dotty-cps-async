package cps.injection.examples.randomgen

trait UniqueSequenceGenerator[F[_], T]{
  def provideMany(n: Int): F[Iterable[T]]
  def provideOne: F[T]
}

