package cps

/**
 * CpsMonadConversion -- conversion from `F[_]` to `G[_]`.
 *  If the given instance of such morphism exists, then `await[F]` can be used inside `async[G]`
 *@see [[cps.await]], [[cps.async]], [[cps.CpsMonad]]
 **/
trait CpsMonadConversion[F[_],G[_]]:
  def apply[T](ft:F[T]): G[T]


