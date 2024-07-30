package injection.examples.randomgen.simple.generator

trait UniqueSequenceGenerator[F[_], T] {
  def generate(startExclusive: Int, endInclusive: Int)(n: Int): F[List[T]]

  def generate(endInclusive: Int)(n: Int): F[List[T]] = generate(0, endInclusive)(n)
}

