package cps

import scala.quoted._
import scala.quoted.matching._


case class TransformationContext[F[_],T](
   patternCode: Expr[T],  // code, for which we build pattern expression
   patternType: Type[T],
   asyncMonad: Expr[AsyncMonad[F]],
)

