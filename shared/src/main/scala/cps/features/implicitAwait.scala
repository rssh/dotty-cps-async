package cps.features

import cps._

object implicitAwait:

  trait IsPossible[F[_]]

  inline given conversion[F[_],T](using CpsMonad[F], IsPossible[F]): Conversion[F[T],T] =
           x => await(x)

