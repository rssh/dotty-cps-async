package cps

object implicitAwait:

  inline given conversion[F[_],T](using CpsMonad[F]) as Conversion[F[T],T] =
           x => await(x) 

