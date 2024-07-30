package injection.examples.randomgen.repository

trait Repository[F[_], T] {
  def all: F[List[T]]
  
  def count: F[Long]
}
