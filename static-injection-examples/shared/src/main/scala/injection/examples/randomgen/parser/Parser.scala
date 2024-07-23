package injection.examples.randomgen.parser

trait Parser[-T, +R] {
  def parse(t: T): R
}


