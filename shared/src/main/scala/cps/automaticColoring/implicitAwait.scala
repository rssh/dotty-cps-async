package cps.automaticColoring

import cps._


trait IsPossible[F[_]]

transparent inline given conversion[F[_],T](using CpsMonad[F], IsPossible[F]): Conversion[F[T],T] =
           x => await(x)


