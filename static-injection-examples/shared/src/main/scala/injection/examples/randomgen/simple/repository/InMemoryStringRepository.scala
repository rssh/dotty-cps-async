package injection.examples.randomgen.simple.repository

import cats.effect.IO

class InMemoryStringRepository(list: List[String]) extends Repository[IO, String] {
  
  override val all: IO[List[String]] = IO.pure(list)

  override val count: IO[Long] = all.map(_.size)
}
