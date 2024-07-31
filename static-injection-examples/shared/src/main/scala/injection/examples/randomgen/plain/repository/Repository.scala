package injection.examples.randomgen.plain.repository

trait Repository[F[_], T] {
  def all: F[List[T]]

  def count: F[Long]
}
