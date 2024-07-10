package cps.injection.examples.randomgen

case class PureEffect[T](value: T) {

  def map[U](f: T => U): PureEffect[U] = {
    PureEffect(f(value))
  }

  def flatMap[U](f: T => PureEffect[U]): PureEffect[U] = {
    f(value)
  }
}
