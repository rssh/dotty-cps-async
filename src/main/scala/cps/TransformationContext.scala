package cps

import scala.quoted._

case class TransformationContext[F[_],T](
   patternCode: Expr[T],  // code, for which we build pattern expression
   patternType: Type[T],
   monad: Expr[CpsMonad[F]],
   flags: AsyncMacroFlags,
   marker: TransformationContextMarker,
   nesting: Int,
   parent: Option[TransformationContext[_,_]],
)  {

  def nestSame(marker: TransformationContextMarker): TransformationContext[F,T] = 
           copy(marker=marker, nesting=nesting+1, parent=Some(this))

  def nest[S](newPatternCode: Expr[S], newPatternType: Type[S], marker: TransformationContextMarker): 
             TransformationContext[F,S] =
      TransformationContext(newPatternCode, newPatternType, monad, flags, 
                             marker, nesting + 1, parent=Some(this) )

  def log(message:String): Unit =
       print("  "*nesting)   
       println(message)   


}

