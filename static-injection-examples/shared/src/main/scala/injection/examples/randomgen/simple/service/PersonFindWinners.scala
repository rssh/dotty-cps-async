package injection.examples.randomgen.simple.service

import cats.effect.Sync
import injection.examples.randomgen.simple.generator.IntUniqueSequenceGenerator
import injection.examples.randomgen.simple.model.Person
import injection.examples.randomgen.simple.parser.Parser
import injection.examples.randomgen.simple.repository.Repository

class PersonFindWinners[F[_] : Sync](stringRepository: Repository[F, String],
                                     indexGenerator: IntUniqueSequenceGenerator[F],
                                     parser: Parser[String, Person]) extends FindWinners[F, Person] {
  override def apply(n: Int): F[List[Person]] =
    Sync[F].flatMap(stringRepository.all)(strings =>
      Sync[F].map(indexGenerator.generate(strings.size)(n))(n => n.map(strings).map(parser.parse))
    )
}
