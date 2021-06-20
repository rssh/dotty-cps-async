package cps.automaticColoring

import cps._

transparent inline given conversion[F[_],T](using CpsAwaitable[F], CpsMonadMemoization[F], AutomaticColoringTag[F]): Conversion[F[T],T] =
           x => await[F,T](x)

//transparent inline given conversion[F[_],T](using CpsMonad[F]): Conversion[F[T],T] =
//           x => await(x)

