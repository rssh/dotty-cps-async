package cps.injection.examples.randomgen

trait UniqueSequenceGenerator[F[_], T]{
  def provideMany(range: Range)(n: Int): F[Iterable[T]]
  def provideOne(range: Range): F[T]
}

