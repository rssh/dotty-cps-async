package cps.runtime

object PartialFunctionHelper {

  def create[X, Y](isDefinedAtF: X => Boolean, applyF: X => Y): PartialFunction[X, Y] =
    new PartialFunction[X, Y] {
      override def isDefinedAt(x: X): Boolean = isDefinedAtF(x)
      override def apply(x: X): Y = applyF(x)
    }

}
