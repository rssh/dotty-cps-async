package injection.examples.randomgen.simple.repository

trait Repository[F[_], T] {
  def all: F[List[T]]

  def count: F[Long]
}
