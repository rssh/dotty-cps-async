package cps.automaticColoring

import cps.*

/**
 * implicit conversion which inserted during autmatic coloring when async value used in sync context.
 **/
@Deprecated("using CpsDirect[F] context function approach instead automatic coloring")
transparent inline given conversion[F[_],T,G[_]](using CpsMonadContext[G], CpsMonadConversion[F,G], CpsMonadMemoization[F], AutomaticColoringTag[F]): Conversion[F[T],T] =
           x => await[F,T,G](x)


