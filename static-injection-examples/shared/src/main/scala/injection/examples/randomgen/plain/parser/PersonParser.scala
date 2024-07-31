package injection.examples.randomgen.plain.parser

import injection.examples.randomgen.plain.model.Person

object PersonParser extends Parser[String, Person] {
  def parse(string: String): Person = Person(string)
}

given Parser[String, Person] = PersonParser
