package cps


given ComputationBoundIsPossible: automaticColoring.Enabled[ComputationBound] with {}

given CpsMonadInplaceMemoization[ComputationBound] with

   def apply[T](ft:ComputationBound[T]): ComputationBound[T] =
      ComputationBound.eagerMemoize(ft)




