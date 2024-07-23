package injection.examples.randomgen.writer

trait Writer[F[_], T] {
  def write(value: T): F[Unit]
}
