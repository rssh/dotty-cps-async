package cps.injection.examples.randomgen

import cats.effect.{IO, Resource}
import org.http4s.circe.CirceEntityCodec.given
import org.http4s.client.Client
import org.http4s.implicits.*

abstract class IntUniqueSequenceGenerator[F[_]](start: Int, end: Int) extends UniqueSequenceGenerator[F, Int]


class DefaultIntUniqueSequenceGenerator(start: Int, end: Int) extends IntUniqueSequenceGenerator[IO](start, end) {
  def provideMany(n: Int): IO[Iterable[Int]] = {
    IO {
      val rangeList = (start until end).toList
      util.Random.shuffle(rangeList).take(n)
    }
  }

  def provideOne: IO[Int] = {
    IO.pure(util.Random.between(start, end))
  }
}

class RandomOrgUniqueSequenceGenerator(start: Int, end: Int) extends IntUniqueSequenceGenerator[IO](start, end) {
  def provideMany(n: Int): IO[Iterable[Int]] = {
    clientResource.use { client =>
      client.expect[List[Int]](
        uri"https://www.random.org/sequences"
          +? ("min" -> start)
          +? ("max" -> end)
          +? ("format" -> "plain")
          +? ("rnd" -> "new")
      ).map(_.take(n))
    }
  }

  //?num=10&min=1&max=6&col=1&base=10&format=plain&rnd=new
  def provideOne: IO[Int] =
    clientResource.use { client =>
      client.expect[Int](
        uri"https://www.random.org/integers"
          +? ("num" -> 1)
          +? ("min" -> start)
          +? ("max" -> end)
          +? ("format" -> "plain")
          +? ("rnd" -> "new")
      )
    }

  private def clientResource: Resource[IO, Client[IO]] =
    org.http4s.blaze.client.BlazeClientBuilder[IO].resource
}