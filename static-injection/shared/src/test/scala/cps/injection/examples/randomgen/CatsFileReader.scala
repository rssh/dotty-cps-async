package cps.injection.examples.randomgen

import cats.effect.{IO, Resource}

import java.io.File
import scala.io.Source

class CatsFileReader(file: File) extends FileReader[IO](file) {

  val resource: Resource[IO,Source] = Resource.make(IO(Source.fromFile(file)))(src => IO(src.close()))

  def readAll(file: File): IO[String] = resource.use { source =>
    IO(source.mkString)
  }

  def readAllLines(file: File): IO[List[String]] =
    resource.use { source =>
      IO(source.getLines.toList)
    }
}
