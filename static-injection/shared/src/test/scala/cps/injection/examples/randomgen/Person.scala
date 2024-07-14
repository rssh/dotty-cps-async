package cps.injection.examples.randomgen

import cats.effect.IO
import cats.syntax.all.*

import java.io.File

case class Person(underlying: String)

object Person {
  def parser: Parser[String, Person, Throwable] = new Parser[String, Person, Throwable] {
    def parse(str: String): Either[Throwable, Person] = Right(Person(str))
  }

  def service(using fileReader: FileReader[IO], intGen: IntUniqueSequenceGenerator, parser: Parser[String, Person, Throwable]): Service[IO, Person, Throwable] = (file: File, n: Int) => {
    for {
      strings <- fileReader.readAllLines(file)
      intSeq <- intGen.provideMany(strings.indices)(n)
    } yield intSeq.map(strings).map(parser.parse)
  }.map(_.sequence)
}
