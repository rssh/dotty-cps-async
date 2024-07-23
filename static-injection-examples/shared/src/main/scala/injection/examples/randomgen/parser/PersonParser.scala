package injection.examples.randomgen.parser

import injection.examples.randomgen.model.Person

object PersonParser extends Parser[String, Person] {
  def parse(string: String): Person = Person(string)
}
