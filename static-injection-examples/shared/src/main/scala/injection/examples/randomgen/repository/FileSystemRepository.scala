package injection.examples.randomgen.repository

import cats.effect.{IO, Resource}

import java.io.File
import scala.io.Source

class FileSystemRepository(file: File) extends Repository[IO, String] {
  private def resource(file: File): Resource[IO, Source] = Resource.make(IO(Source.fromFile(file)))(src => IO(src.close()))

  override def all: IO[List[String]] = resource(file).use { source =>
    IO(source.getLines.toList)
  }
}
