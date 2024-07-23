package injection.examples.randomgen

import cats.effect.{ExitCode, IO, IOApp}
import injection.examples.randomgen.generator.ScalaRandomIntUniqueSequenceGenerator
import injection.examples.randomgen.parser.PersonParser
import injection.examples.randomgen.repository.InMemoryStringRepository
import injection.examples.randomgen.service.PersonFindWinners
import injection.examples.randomgen.writer.ConsoleWriter

object Main extends IOApp {
  override def run(args: List[String]): IO[ExitCode] =
    val num = 3
    val repository = new InMemoryStringRepository(List("a", "b", "c", "d", "e"))
    val indexGenerator = ScalaRandomIntUniqueSequenceGenerator
    val parser = PersonParser
    new PersonFindWinners(repository, indexGenerator, parser)(num)
      .flatMap(ConsoleWriter.write)
      .map(_ => ExitCode.Success)
}
