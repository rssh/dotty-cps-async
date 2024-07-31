package injection.examples.randomgen.simple.parser

trait Parser[-T, +R] {
  def parse(t: T): R
}


