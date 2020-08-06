package cps


object implicitAwait:

  trait IsPossible[T[_]]

  inline given conversion[F[_],T](using CpsMonad[F], IsPossible[F]) as Conversion[F[T],T] =
           x => await(x) 

