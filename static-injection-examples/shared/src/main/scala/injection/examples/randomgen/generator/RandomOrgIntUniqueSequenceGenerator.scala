package injection.examples.randomgen.generator

import cats.effect.{IO, Resource}
import org.http4s.circe.CirceEntityCodec.given
import org.http4s.client.Client
import org.http4s.implicits.*

object RandomOrgIntUniqueSequenceGenerator extends IntUniqueSequenceGenerator {
  override def generate(startExclusive: Int, endInclusive: Int)(n: Int): IO[List[Int]] = client.use { client =>
    client.expect[List[Int]](
      uri"https://www.random.org/sequences"
        +? ("min" -> startExclusive)
        +? ("max" -> endInclusive)
        +? ("format" -> "plain")
        +? ("rnd" -> "new")
    ).map(_.take(n))
  }

  private def client: Resource[IO, Client[IO]] = org.http4s.blaze.client.BlazeClientBuilder[IO].resource
}
