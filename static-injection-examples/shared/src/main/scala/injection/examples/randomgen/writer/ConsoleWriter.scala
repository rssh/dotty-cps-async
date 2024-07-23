package injection.examples.randomgen.writer

import cats.effect.IO
import injection.examples.randomgen.model.Person

object ConsoleWriter extends Writer[IO, List[Person]] {
  def write(value: List[Person]): IO[Unit] = IO.println(value)
}
