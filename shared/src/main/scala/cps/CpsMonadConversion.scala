package cps

trait CpsMonadConversion[F[_],G[_]]:
  def apply[T](ft:F[T]): G[T]


