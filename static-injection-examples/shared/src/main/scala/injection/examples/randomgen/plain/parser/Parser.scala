package injection.examples.randomgen.plain.parser

trait Parser[-T, +R] {
  def parse(t: T): R
}


