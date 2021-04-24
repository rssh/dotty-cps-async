package cps.automaticColoring

import cps._


transparent inline given conversion[F[_],T](using CpsMonad[F], CpsMonadMemoization[F]): Conversion[F[T],T] =
           x => await(x)

