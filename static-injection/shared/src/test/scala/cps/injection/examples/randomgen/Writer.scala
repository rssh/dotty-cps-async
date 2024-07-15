package cps.injection.examples.randomgen

trait Writer[F[_], T] {
  def write(value: T): F[Unit]
}
