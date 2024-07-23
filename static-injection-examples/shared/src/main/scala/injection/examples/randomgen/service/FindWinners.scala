package injection.examples.randomgen.service

import cats.effect.IO

import java.io.File

trait FindWinners[F[_], T] {
  def apply(n: Int): F[List[T]]
}
