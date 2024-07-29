package injection.examples.randomgen

import cats.effect.{ExitCode, IO, IOApp}
import cps.monads.catsEffect.given
import injection.examples.randomgen.generator.RandomOrgIntUniqueSequenceGenerator
import injection.examples.randomgen.parser.PersonParser
import injection.examples.randomgen.repository.InMemoryStringRepository
import injection.examples.randomgen.service.PersonFindWinners
import cps.{*, given}

object Main extends IOApp {
  override def run(args: List[String]): IO[ExitCode] = async[IO] {
    val num = args.head.toInt
    val finder = await(new PersonFindWinners(
      new InMemoryStringRepository(List("a", "b", "c", "d", "e")),
      RandomOrgIntUniqueSequenceGenerator,
      PersonParser
    )(num))
    await(IO.println(finder))
    ExitCode.Success
  }
}
