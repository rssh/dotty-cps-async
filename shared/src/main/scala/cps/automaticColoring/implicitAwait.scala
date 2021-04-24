package cps.automaticColoring

import cps._


trait Enabled[F[_]]

transparent inline given conversion[F[_],T](using CpsMonad[F], Enabled[F]): Conversion[F[T],T] =
           x => await(x)


