package cps.injection.examples.randomgen

import cats.effect.{IO, Resource}

import java.io.File
import scala.io.Source

abstract class FileReader[F[_]] {
  def readAll(file: File): F[String]
  def readAllLines(file: File): F[List[String]]
}

object FileReader {
  def cats: FileReader[IO] = new FileReader[IO] {

    def resource(file: File): Resource[IO, Source] = Resource.make(IO(Source.fromFile(file)))(src => IO(src.close()))

    def readAll(file: File): IO[String] = resource(file).use { source =>
      IO(source.mkString)
    }

    def readAllLines(file: File): IO[List[String]] = resource(file).use { source =>
      IO(source.getLines.toList)
    }
  }
}


