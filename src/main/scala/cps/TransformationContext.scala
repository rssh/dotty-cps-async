package cps

import scala.quoted._
import scala.quoted.matching._


case class TransformationContext[F[_],T](
   patternCode: Expr[T],  // code, for which we build pattern expression
   patternType: Type[T],
   monad: Expr[CpsMonad[F]],
   flags: AsyncMacroFlags,
   exprMarker: String,
   nesting: Int
)  {

  def nestSame(s:String): TransformationContext[F,T] = 
           copy(exprMarker=exprMarker+s, nesting=nesting+1)

  def nest[S](newPatternCode: Expr[S], newPatternType: Type[S], s: String): 
             TransformationContext[F,S] =
      TransformationContext(newPatternCode, newPatternType, monad, flags, 
                             exprMarker+s, nesting + 1 )

}

