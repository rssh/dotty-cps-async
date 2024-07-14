package cps.injection.examples.randomgen

trait Parser[-T, +R, +E] {
  def parse(t: T): Either[E, R]
}


