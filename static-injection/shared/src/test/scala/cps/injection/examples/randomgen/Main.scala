package cps.injection.examples.randomgen

import cats.effect.{ExitCode, IO, IOApp}

import java.io.File

object Main extends IOApp {
  override def run(args: List[String]): IO[ExitCode] = for {
    (firstArg, secondArg) <- IO(args(0) -> args(1))
    file <- IO(new File(firstArg))
    num <- IO(secondArg.toInt)
    _ <- Person.service(FileReader.cats, RandomOrgUniqueSequenceGenerator, Person.parser).findWinners(file, num)
  } yield ExitCode.Success
}
