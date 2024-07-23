package injection.examples.randomgen.repository

import cats.effect.IO

class InMemoryStringRepository(list: List[String]) extends Repository[IO, String] {
  val all: IO[List[String]] = IO.pure(list)
}
