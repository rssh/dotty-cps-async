package cps.injection.examples.randomgen

import cats.effect.IO

import java.io.File

abstract class Service[F[_], T] {
  def findWinners(file: File, n: Int): F[T]
}

object Service {
  def of[T](using fileReader: FileReader[IO], intGen: IntUniqueSequenceGenerator): Service[IO, T] = new Service[IO, T] {
    def findWinners(file: File, n: Int): IO[T] = {
      for {
        strings <- fileReader.readAllLines(file)
      } yield ???
    }
  }
}
