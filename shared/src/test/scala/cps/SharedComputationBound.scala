package cps

import automaticColoring._

given ComputationBoundIsPossible: IsPossible[ComputationBound] with {}


inline transparent given ResolveMonadMemoizationKind[ComputationBound] =
                                             ResolveMonadMemoizationKind(MonadMemoizationKind.INPLACE)

given CpsMonadInplaceMemoization[ComputationBound] with

   def apply[T](ft:ComputationBound[T]): ComputationBound[T] =
      ComputationBound.eagerMemoize(ft)




