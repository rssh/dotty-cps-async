package cps.injection.examples.randomgen

import cats.effect.IO

class ConsoleWriter extends Writer[IO, List[String]] {
  def write(value: List[String]): IO[Unit] = IO.delay(Console.print(value))
}
