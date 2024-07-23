package injection.examples.randomgen.service

import cats.effect.IO
import injection.examples.randomgen.generator.IntUniqueSequenceGenerator
import injection.examples.randomgen.model.Person
import injection.examples.randomgen.parser.Parser
import injection.examples.randomgen.repository.Repository

class PersonFindWinners(stringRepository: Repository[IO, String],
                        indexGenerator: IntUniqueSequenceGenerator,
                        parser: Parser[String, Person]) extends FindWinners[IO, Person] {
  override def apply(n: Int): IO[List[Person]] = for {
    strings <- stringRepository.all
    indices <- indexGenerator.generate(strings.size)(n)
  } yield {
    indices.map(strings).map(parser.parse)
  }
}
