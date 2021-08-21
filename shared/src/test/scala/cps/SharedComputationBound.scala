package cps


given CpsMonadMemoization.Inplace[ComputationBound] with

   def apply[T](ft:ComputationBound[T]): ComputationBound[T] =
      ComputationBound.eagerMemoize(ft)




