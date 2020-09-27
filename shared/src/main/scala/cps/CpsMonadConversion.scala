package cps

/**
 * You should implement this interface and made given instance available
 * for using await[F] in async[G]
 **/
trait CpsMonadConversion[F[_],G[_]]:

   def apply[T](mf:CpsMonad[F],mg:CpsMonad[G],ft:F[T]):G[T]



