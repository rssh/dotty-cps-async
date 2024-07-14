package cps.injection.examples.randomgen

import cats.effect.{IO, Resource}
import org.http4s.circe.CirceEntityCodec.given
import org.http4s.client.Client
import org.http4s.implicits.*

type IntUniqueSequenceGenerator = UniqueSequenceGenerator[IO, Int]

class DefaultIntUniqueSequenceGenerator extends IntUniqueSequenceGenerator{
  def provideMany(range: Range)(n: Int): IO[Seq[Int]] = IO {
    val rangeList = (range.start until range.end).toList
    util.Random.shuffle(rangeList).take(n)
  }

  def provideOne(range: Range): IO[Int] = IO.pure(util.Random.between(range.start, range.end))
}

class RandomOrgUniqueSequenceGenerator extends IntUniqueSequenceGenerator {
  def provideMany(range: Range)(n: Int): IO[Seq[Int]] = clientResource.use { client =>
    client.expect[List[Int]](
      uri"https://www.random.org/sequences" 
        +? ("min" -> range.start) 
        +? ("max" -> range.end) 
        +? ("format" -> "plain")
        +? ("rnd" -> "new")
    ).map(_.take(n))
  }

  //?num=10&min=1&max=6&col=1&base=10&format=plain&rnd=new
  def provideOne(range: Range): IO[Int] = clientResource.use { client =>
    client.expect[Int](
      uri"https://www.random.org/integers"
        +? ("num" -> 1)
        +? ("min" -> range.start)
        +? ("max" -> range.end)
        +? ("format" -> "plain")
        +? ("rnd" -> "new")
    )
  }

  private def clientResource: Resource[IO, Client[IO]] = org.http4s.blaze.client.BlazeClientBuilder[IO].resource
}