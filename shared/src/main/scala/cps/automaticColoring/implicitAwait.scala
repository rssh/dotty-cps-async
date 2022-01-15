package cps.automaticColoring

import cps.*

/**
 * implicit conversion which inserted during autmatic coloring when async value used in sync context.
 **/
transparent inline given conversion[F[_],T,C <: CpsMonadContext[?]](using CpsAwaitable[F], CpsMonadMemoization[F], AutomaticColoringTag[F], C): Conversion[F[T],T] =
           x => await[F,T,C](x)


