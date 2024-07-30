package injection.examples.randomgen.simple.parser

import injection.examples.randomgen.simple.model.Person

object PersonParser extends Parser[String, Person] {
  def parse(string: String): Person = Person(string)
}
