package cps.injection.examples.randomgen.provider

import cats.effect.IO
import cps.injection.examples.randomgen.PureEffect
import cats.syntax.all.*
import org.http4s.client.Client
import org.http4s.implicits.uri

abstract class IntRandomProvider[F[_]](start: Int, end: Int) extends RandomProvider[F, Int]

class DefaultIntRandomProvider(start: Int, end: Int) extends IntRandomProvider[IO](start, end) {
  def provideMany(n: Int): IO[Iterable[Int]] = {
    (1 to n).toList.map(_ => provideOne).sequence
  }

  def provideOne: IO[Int] = {
    IO.pure(util.Random.between(start, end))
  }
}

class RandomOrgIntProvider(start: Int, end: Int) extends IntRandomProvider[IO](start, end) {
  def provideMany(n: Int): IO[Iterable[Int]] = {
    ???
  }

  def provideOne: IO[Int] = {
    ???
  }

  def callEffect(client: Client[IO], n: Int): IO[List[Int]] =
    client.expect[List[Int]](uri"https://www.random.org/integers" +? ""  )
}

object IntRandomProvider {

}
