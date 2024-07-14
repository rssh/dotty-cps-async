package cps.injection.examples.randomgen

trait UniqueSequenceGenerator[F[_], T]{
  def provideMany(range: Range)(n: Int): F[Seq[T]]
  def provideOne(range: Range): F[T]
}

